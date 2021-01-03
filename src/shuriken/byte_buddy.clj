(ns shuriken.byte-buddy
  (:refer-clojure :exclude [require name merge not load resolve])
  (:require [clojure.core :as clj]
            [clojure.string :as str]
            [com.palletops.ns-reload :refer [reload-namespaces]]
            [shuriken.associative :refer [getsoc]]
            [shuriken.exception :refer [silence]]
            [shuriken.namespace :refer [import-namespace-vars]]
            [shuriken.reflection :refer [read-field write-field class-tree]]
            [shuriken.sequential :refer [forcatm mapm maps]]
            [threading.core :refer :all]
            [weaving.core :refer :all]
            [shuriken.byte-buddy.dsl])
  (:import [clojure.lang DynamicClassLoader]
           [java.io ByteArrayInputStream]
           [net.bytebuddy.dynamic
            DynamicType
            DynamicType$Default
            DynamicType$Default$Unloaded
            DynamicType$Builder
            ClassFileLocator$Simple
            TypeResolutionStrategy$Passive]
           [net.bytebuddy.dynamic.loading
            ClassLoadingStrategy$Default]
           [net.bytebuddy.pool
            TypePool$ClassLoading]
           [javassist
            CtClass
            ClassPool
            Loader
            Loader$Simple]))

(import-namespace-vars shuriken.byte-buddy.dsl)

;; TODO: only copy-class! is exposed for now.
;; TODO: refactor the .defrost to CtClasses

(defn ->javassist [^DynamicType type]
  (let [pool        (doto (new ClassPool) (.appendSystemPath))
                    ;(ClassPool/getDefault)
        class-name  (-> type .getTypeDescription .getName)
        _           (doto (silence javassist.NotFoundException
                            (.get pool class-name))
                          (some-> (when-> .isFrozen .defrost)))
        result      (doto (->> type .getBytes ByteArrayInputStream.
                               (.makeClass pool))
                          (when-> .isFrozen .defrost))]
    result))

(defn ->byte-buddy
  ([ct-class]
   (throw (Exception. "Not implemented"))
   ; (let [thread-cl (.getContextClassLoader (Thread/currentThread))
   ;       tmp-cl (DynamicClassLoader. thread-cl)
   ;       klass  (.toClass ct-class tmp-cl nil
   ;                        #_(-> ct-class. originalClass .getProtectionDomain))
   ;       result (-> (buddy)
   ;                  (with net.bytebuddy.implementation.attribute.AnnotationValueFilter$Default/SKIP_DEFAULTS)
   ;                  (redefine klass (class-file-locator :class-loader tmp-cl))
   ;                  (make))
   ;       ct-bytecode (seq (.toBytecode ct-class))
   ;       bb-bytecode (seq (.getBytes result))
   ;       ct2-bytecode (seq (.toBytecode (->javassist result)))]
   ;   (println (map vector ct-bytecode bb-bytecode ct2-bytecode))
   ;   result)

   ; (let [ct-bytecode (seq (.toBytecode ct-class))
   ;       classes (->> (class-tree :nested ct-class)
   ;                    flatten
   ;                    (map #(doto % .defrost))
   ;                    (mapm (juxt #(type-description :javassist %)
   ;                                (memfn ^CtClass toBytecode))))
   ;       locator (class-file-locator :compound [:simple classes] [:class-loader])
   ;       desc    (type-description :javassist ct-class locator)
   ;       result  (-> (buddy)
   ;                   (redefine desc locator)
   ;                   make)
   ;       bb-bytecode (seq (.getBytes result))
   ;       ct2-bytecode (-> result ->javassist .toBytecode seq)]
   ;   (println (map vector ct-bytecode bb-bytecode ct2-bytecode))
   ;   result)
   )
  ([ct-class original-type]
   (let [bytecode (.toBytecode ct-class)]
     (DynamicType$Default$Unloaded.
       (.getTypeDescription original-type)
       bytecode
       (.get (.getLoadedTypeInitializers original-type)
             (.getTypeDescription original-type))
       (read-field original-type 'auxiliaryTypes)
       TypeResolutionStrategy$Passive/INSTANCE))))

;; Taken from Pomegranate
(defn- ensure-compiler-loader!
  "Ensures the clojure.lang.Compiler/LOADER var is bound to a DynamicClassLoader,
  so that we can add to Clojure's classpath dynamically."
  []
  (when-not (bound? Compiler/LOADER)
    (.bindRoot Compiler/LOADER (DynamicClassLoader. (clojure.lang.RT/baseLoader)))))

; (defn reimport-classes! [classes]
;   (let [class-names (maps #(.getName %) classes)]
;     (-> (all-ns)
;         (map-> (juxt-> identity
;                        (->> ns-imports
;                             (filter #(contains? class-names (.getName (val %))))
;                             (>>- (map-> vec))
;                             (into {})))
;                vec)
;         (->> (filter #(-> % second seq))
;              (into {})
;              (run! (fn [[ns class-map]]
;                      (run! (fn [[sym klass]]
;                              (.importClass ns sym klass))
;                            class-map)))))))

(defn reload-ns-importing-classes! [classes]
  (let [class-names (maps #(.getName %) classes)]
    (-> (all-ns)
        (map-> (juxt-> identity
                       (->> ns-imports
                            (filter #(contains? class-names (.getName (val %))))
                            (>>- (map-> vec))
                            (into {})))
               vec)
        (->> (filter #(-> % second seq)))
        (map-> first .getName)
        (reload-namespaces {}))))

;; TODO: extract and reuse ?
(defn- frames
  ([]
   (->> (iterate #(read-field % :prev)
                 (.get (read-field clojure.lang.Var :dvals)))
        (take-while identity)
        (map #(read-field % :bindings))))
  ([& vars]
   (map #(select-keys % vars)
        (frames))))

;; TODO: mmh this fiddling with the binding frames is exciting: extract it.
(defn load-in-clojure-classloaders! [type]
  (ensure-compiler-loader!)
  (let [load-type
        (condp instance? type
          DynamicType$Default (fn [class-loader]
                                (-> (load
                                      type
                                      class-loader
                                      (allow-existing-types
                                        (class-loading-strategy
                                          :wrapper-persistent)))
                                    get-loaded))
          CtClass             (fn [class-loader]
                                (-> (Loader$Simple. class-loader)
                                    (.invokeDefineClass type))
                                ; (let [pool (ClassPool/getDefault)
                                ;       loader (Loader. class-loader pool)]
                                ;   (.loadClass loader (.getName type)))

                                ;; (╯°□°）╯︵ ┻━┻  WHY U NO WOK ?
                                ;; String rather than TypeDescription using
                                ;; directly ByteArrayClassLoader ?
                                ; (-> (load
                                ;       (allow-existing-types
                                ;         (class-loading-strategy :wrapper))
                                ;       class-loader
                                ;       {(type-description :javassist type)
                                ;        (.toBytecode type)})
                                ;     vals
                                ;     first)
                                ;; Other way
                                ; (let [class-name (.getName type)]
                                ;   (.loadClass
                                ;     (net.bytebuddy.dynamic.loading.ByteArrayClassLoader.
                                ;       class-loader false {class-name
                                ;                           (.toBytecode type)})
                                ;     class-name))
                                ;; ┬──┬◡ﾉ(° -°ﾉ)
                                ))
        th (Thread/currentThread)]
    (.bindRoot Compiler/LOADER
               (-> @Compiler/LOADER load-type .getClassLoader DynamicClassLoader.))
    (->> (frames Compiler/LOADER)
         (apply concat)
         (map (fn [[_var tbox]] tbox))
         (filter #(-> % (read-field :thread) (= th)))
         (reduce (fn [existing tbox]
                   (let [cl (read-field tbox :val)
                         [new-cl new-existing]
                         (getsoc existing cl
                                 (-> cl load-type .getClassLoader DynamicClassLoader.))]
                     (write-field tbox :val new-cl)
                     new-existing))
                 {}))
    (let [loaded-class (-> th .getContextClassLoader load-type)]
      (.setContextClassLoader
        th (-> loaded-class .getClassLoader DynamicClassLoader.))
      #_(reload-ns-importing-classes!
        (flatten (class-tree :nested loaded-class)))
      loaded-class)))

;; TODO: expose a way to define which (sub)classes are rewritten
(defn deep-copy [class new-name]
  (let [ctree (class-tree :nested class)
        prefix-pattern (->> class .getName str/re-quote-replacement (str "^")
                            re-pattern)
        new-name (clj/name new-name)
        get-copy-name (fn [class]
                        (str/replace (.getName class)
                                     prefix-pattern
                                     new-name))
        class-builder-dict (forcatm [class (flatten ctree)
                                     :let [copy-name (get-copy-name class)
                                           copy-builder (-> (buddy)
                                                            (.with net.bytebuddy.implementation.attribute.AnnotationValueFilter$Default/SKIP_DEFAULTS)
                                                            (redefine class)
                                                            (name copy-name))]]
                             [[class copy-builder]
                              [copy-name  copy-builder]])
        class-name-dict (->> (keys class-builder-dict)
                             (filter class?)
                             (mapm #(do [(.getName %) (get-copy-name %)])))
        new-class-names (->> (keys class-builder-dict)
                             (filter string?))
        rename-other-classes (fn [ct-class]
                               (let [other-classes (dissoc class-name-dict
                                                           (.getName ct-class))]
                                 (doseq [[old-name new-name] other-classes]
                                   (.replaceClassName ct-class old-name new-name))
                                 ct-class))
        nest (fn [copy-builder nested-copies]
               (let [nested-types (maps (memfn ^DynamicType getTypeDescription)
                                        nested-copies)
                     type-with-nested (-> copy-builder
                                          (when-> (<- (seq nested-copies))
                                            (declared-types nested-types)
                                            (require nested-copies)
                                            #_(nest-members nested-types))
                                          make)
                     type-with-renames (-> type-with-nested
                                           ->javassist
                                           rename-other-classes
                                           (->byte-buddy type-with-nested)
                                           (when-> (<- (seq nested-copies))
                                             (include nested-copies)))]
                 type-with-renames))
        maybe-make (when| (|| instance? DynamicType$Builder) make)]
    (clojure.walk/postwalk
      (fn [node]
        (cond
          (class? node)       (-> node class-builder-dict)
          (sequential? node)  (let [[mother & children] node]
                                (-> (map maybe-make children)
                                    (if->> seq
                                      (nest mother)
                                      (<<- (maybe-make mother)))))
          :else               node))
      ctree)))

(defn copy-class! [class new-name]
  (-> (deep-copy class new-name)
      load-in-clojure-classloaders!))
