(ns shuriken.byte-buddy
  (:use clojure.pprint shuriken.macro)
  (:require [clojure.string :as str]
            [shuriken.associative :refer [getsoc]]
            [shuriken.exception :refer [silence]]
            [shuriken.reflection :refer [read-field write-field]]
            [shuriken.tree :refer [class-tree]]
            [threading.core :refer :all]
            [weaving.core :refer :all]
            [shuriken.byte-buddy.dsl
             :refer :all
             :exclude [require name merge not load]
             :as dsl])
  (:import [clojure.lang DynamicClassLoader]
           [java.io ByteArrayInputStream]
           [net.bytebuddy.dynamic
            DynamicType
            DynamicType$Default
            DynamicType$Default$Unloaded
            DynamicType$Builder
            ClassFileLocator$Simple
            TypeResolutionStrategy$Passive]
           [net.bytebuddy.pool
            TypePool$ClassLoading]
           [javassist
            CtClass
            ClassPool]))

;; TODO: only copy-class! is exposed for now.

;; TODO: move and generalize
(defmacro forcat [& args]
  `(apply concat (for ~@args)))

(defmacro forcatm [& args]
  `(into {} (forcat ~@args)))

(defmacro mapm [& args]
  `(into {} (map ~@args)))

(defmacro maps [& args]
  `(set (map ~@args)))

(defn ->javassist [^DynamicType type]
  (let [pool (ClassPool/getDefault)
        name (-> type .getTypeDescription .getName)
        existing-type (silence javassist.NotFoundException (.get pool name))
        result (or existing-type (->> type .getBytes ByteArrayInputStream.
                                      (.makeClass pool)))]
    (when-> result .isFrozen .defrost)
    result))

(defn ->byte-buddy [^javassist.CtClass            ctclass
                    ^DynamicType$Default$Unloaded original-type
                    & {:keys [type-resolution-strategy]
                       :or {type-resolution-strategy
                            TypeResolutionStrategy$Passive/INSTANCE}}]
  (DynamicType$Default$Unloaded.
    (.getTypeDescription original-type)
    (.toBytecode ctclass)
    (.get (.getLoadedTypeInitializers original-type)
          (.getTypeDescription original-type))
    (read-field original-type 'auxiliaryTypes)
    type-resolution-strategy))

;; Taken from Pomegranate
(defn ensure-compiler-loader!
  "Ensures the clojure.lang.Compiler/LOADER var is bound to a DynamicClassLoader,
  so that we can add to Clojure's classpath dynamically."
  []
  (when-not (bound? Compiler/LOADER)
    (.bindRoot Compiler/LOADER (DynamicClassLoader. (clojure.lang.RT/baseLoader)))))

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

(defn load-in-clojure-classloader! [^DynamicType$Default type]
  (ensure-compiler-loader!)
  (let [load-type #(-> (dsl/load type % (.allowExistingTypes
                                          (class-loading-strategy :wrapper))))
        th (Thread/currentThread)]
    (->> (frames Compiler/LOADER)
         (apply concat)
         (map (fn [[_var tbox]] tbox))
         (filter #(-> % (read-field :thread) (= th)))
         (reduce (fn [existing tbox]
                   (let [cl (read-field tbox :val)
                         [new-cl new-existing]
                         (getsoc existing cl
                                 (-> cl load-type get-loaded
                                     .getClassLoader DynamicClassLoader.))]
                     (write-field tbox :val new-cl)
                     new-existing))
                 {}))
    (.bindRoot Compiler/LOADER (-> @Compiler/LOADER load-type get-loaded
                                   .getClassLoader DynamicClassLoader.))
    (doto (Thread/currentThread)
          (.setContextClassLoader
            (-> (Thread/currentThread) .getContextClassLoader load-type get-loaded
                .getClassLoader DynamicClassLoader.)))
    nil))

;; TODO: expose a way to define which (sub)classes are rewritten
(defn deep-copy [class new-name]
  (let [ctree (class-tree :nested class)
        prefix-pattern (->> class .getName str/re-quote-replacement (str "^")
                            re-pattern)
        new-name (name new-name)
        get-copy-name (memoize (fn [class]
                                 (str/replace (.getName class)
                                              prefix-pattern
                                              new-name)))
        class-builder-dict (forcatm [class (flatten ctree)
                                     :let [copy-name (get-copy-name class)
                                           copy-builder (-> (buddy)
                                                            (redefine class)
                                                            (dsl/name copy-name))]]
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
                                            (dsl/require nested-copies)
                                            #_(nest-members nested-types))
                                          make)
                     type-with-renames (-> type-with-nested
                                           ->javassist
                                           rename-other-classes
                                           (->byte-buddy type-with-nested)
                                           (when-> (<- (seq nested-copies))
                                             (include nested-copies)))]
                 type-with-renames))]
    (clojure.walk/postwalk
      (fn [node]
        (cond
          (class? node)       (-> node class-builder-dict)
          (sequential? node)  (let [[mother & children] node]
                                (-> (map (when| (|| instance? DynamicType$Builder)
                                           make)
                                         children)
                                    (if->> seq
                                      (nest mother)
                                      (<<- mother))))
          :else               node))
      ctree)))

(defn copy-class! [class new-name]
  (-> (deep-copy class new-name)
      load-in-clojure-classloader!))
