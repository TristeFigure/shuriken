(ns shuriken.reflection
  "### Utilities to handle various reflection formats in a unified way"
  (:use clojure.pprint
        shuriken.debug)
  (:require [clojure.spec.alpha :as s]
            [shuriken.spec :refer [conf either conform!]]
            [shuriken.macro :refer [is-form?]]
            [shuriken.exception :refer [capturex]]
            [threading.core :refer :all])
  (:import [java.lang.reflect Method Modifier]
           [clojure.lang      Symbol Reflector]
           [javassist         CtClass]))

;; TODO: document in the README and/or release as an independent lib

(defn ct-class? [x]
  (instance? javassist.CtClass x))

(defn- var-or-class-name [x]
  (if-> x var?
    (-> .sym name)
    .getName))

(defmulti type-to-str class)
(defmethod type-to-str String   [t]  (-> t symbol resolve var-or-class-name))
(defmethod type-to-str Symbol   [t]  (-> t        resolve var-or-class-name))
(defmethod type-to-str Class    [t]  (-> t                .getName))
(defmethod type-to-str CtClass  [t]  (-> t                .getName))
(defmethod type-to-str :default [t]  (name t))

(defn types-to-str [types]
  (->> (map type-to-str types)
       (keep identity)
       vec))

(s/def ::method-signature
       (-> (s/cat
             :class-name      (either
                                :string   string?
                                :class    (conf #(or (class? %) (ct-class? %))
                                                type-to-str #(.getName %))
                                :ident    (conf ident? type-to-str str)
                                :unform   (fn [x]
                                            (cond (string? x) :string
                                                  (class? x)  :class
                                                  (ident? x)  :ident)))
             :method-name     (either
                                :string string?
                                :ident  (conf ident? name)
                                :quote? (conf #(is-form? 'quote %)
                                              #(-> % first str))
                                :unform :string)
             :parameter-types (s/?
                                (conf (s/coll-of
                                        #(or (string? %) (ident? %) (class? %)))
                                      types-to-str)))
           (conf (comp vec (juxt :class-name :method-name :parameter-types))
                 #(zipmap % [:class-name :method-name :parameter-types]))))

(defn signature-to-str [method-signature]
  (let [[class-name method-name parameter-types]
        (conform! ::method-signature method-signature)
        args (remove nil? (concat [class-name method-name]
                                  parameter-types))]
    (-> (clojure.string/join "-" args)
        (clojure.string/replace #"\." "_"))))

(defn find-class [class-or-method-signature]
  (if (ct-class? class-or-method-signature)
    class-or-method-signature
    (let [class-name (cond
                       (class? class-or-method-signature)
                       (  .getName class-or-method-signature)
                       (or (symbol? class-or-method-signature)
                           (string? class-or-method-signature))
                       (  str class-or-method-signature)
                       :else (-> (conform! ::method-signature
                                           class-or-method-signature)
                                 first))
          class-pool (javassist.ClassPool/getDefault)
          ct-class (.get class-pool class-name)]
        (assert ct-class "Class not found")
        (.stopPruning ct-class true)
        ct-class)))

(defn find-method [method-signature]
  (let [[_class-name method-name parameter-types]
        (conform! ::method-signature method-signature)
        ct-class (find-class method-signature)
        ct-method (->> (.getMethods ct-class)
                       (filter #(and (= method-name (.getName %))
                                     (if (nil? parameter-types)
                                       true
                                       (= (-> % .getParameterTypes types-to-str)
                                          parameter-types))))
                       first)]
    (assert ct-method "Method not found")
    ct-method))

(defn return-type [method-signature]
  (let [ct-method (find-method method-signature)]
    (-> ct-method .getReturnType .getName)))

(defn delegate-name [method-signature]
  (-> method-signature
      signature-to-str
      (str "-clojure-delegate")
      symbol))

(defn static-method? [x]
  (cond
    (instance? Method x) (-> x .getModifiers Modifier/isStatic)
    (sequential? x)      (-> x find-method .getModifiers Modifier/isStatic)))

;; TODO: some optimizations can be made by turning the following into
;; a macro.
(defn read-field
  "Returns the value of a field, static if x is a class, or on the
  passed instance otherwise. All fields, private, protected or public,
  are made accessible."
  [x nme]
  (let [c (if (class? x) x (class x))
        candidates (->> (ancestors c)
                        (cons c)
                        (remove (memfn ^Class isInterface))
                        (map #(capturex NoSuchFieldException
                                (doto (.getDeclaredField % (name nme))
                                      (.setAccessible true)))))]
    (-> (or (some #(when-not (instance? NoSuchFieldException %) %)
                  candidates)
            (throw (first candidates)))
        (.get x))))

(defn write-field
  "Sets the value of a field, static if x is a class, or on the
  passed instance otherwise. All fields, private, protected or public,
  are made accessible."
  [x nme v]
  (let [c (if (class? x) x (class x))
        candidates (->> (ancestors c)
                        (cons c)
                        (remove (memfn ^Class isInterface))
                        (map #(capturex NoSuchFieldException
                                (doto (.getDeclaredField % (name nme))
                                      (.setAccessible true)
                                      (.set x v)))))]
    (-> (or (some #(when-not (instance? NoSuchFieldException %) %)
                  candidates)
            (throw (first candidates)))
        (.get x))))

(defn method
  "Finds a method by reflection."
  [klass nme parameter-types]
  (let [klass (if (class? klass) klass (resolve (-> klass name symbol)))
        m (-> (doto (.getDeclaredMethod klass (name nme)
                                        (into-array Class parameter-types))
                    (.setAccessible true)))]
    (fn [target & args]
      (.invoke m target (to-array args)))))

(defn static-method
  "Finds a static method by reflection."
  [class name parameter-types]
  (let [m (method class name parameter-types)]
    (partial m nil)))

(defn instance-methods [class]
  (remove #(-> % .getModifiers Modifier/isStatic)
          (.getDeclaredMethods class)))

(defn static-methods [class]
  (filter #(-> % .getModifiers Modifier/isStatic)
          (.getDeclaredMethods class)))

(defn dyncall [class-or-instance method-name & args]
  {:pre [(assert (not= Class class-or-instance)
                 "Not available for class Class")]}
  (if (class? class-or-instance)
    (Reflector/invokeStaticMethod
      class-or-instance (name method-name) (into-array Object args))
    (Reflector/invokeInstanceMethod
      class-or-instance (name method-name) (into-array Object args))))

(defn interfaces [^Class class]
  (seq (.getInterfaces class)))
