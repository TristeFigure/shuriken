(ns shuriken.reflection
  (:require [clojure.spec.alpha :as s]
            [shuriken.spec :refer [conf either conform!]]
            [shuriken.macro :refer [is-form?]])
  (:import java.lang.reflect.Modifier))

;; TODO: document in the README and/or release as an independent lib

(defn- ct-class? [x]
  (instance? javassist.CtClass x))

(defn- type-to-str [type]
  (->> (cond
         (string? type)                      type
         (symbol? type)                      (str type)
         (or (class? type) (ct-class? type)) (.getName type)
         :else                               (name type))
       symbol
       resolve
       str
       (re-find #"(?<=class ).*")))

(defn- types-to-str [types]
  (->> (map type-to-str types)
       (keep identity)
       vec))

(s/def ::method-signature
       (-> (s/cat
             :class-name      (either
                                :string string?
                                :class  (conf class? type-to-str #(.getName %))
                                :symbol (conf symbol? type-to-str str)
                                :unform (fn [x]
                                          (cond (string? x) :string
                                                (class? x)  :class
                                                (symbol? x) :symbol)))
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

(defn find-class [method-signature]
  (let [[class-name _method-name _parameter-types]
        (conform! ::method-signature method-signature)
        class-pool (javassist.ClassPool/getDefault)
        ct-class (.get class-pool class-name)]
    (assert ct-class "Class not found")
    (.stopPruning ct-class true)
    ct-class))

(defn find-method [method-signature]
  (let [[_class-name method-name parameter-types]
        (conform! ::method-signature method-signature)
        ct-class (find-class method-signature)
        ct-method (->> (.getMethods ct-class)
                       (filter
                         #(and (= method-name (.getName %))
                               (if (nil? parameter-types)
                                 true
                                 (= (->> % .getParameterTypes types-to-str)
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

(defn static-method? [method-signature]
  (-> method-signature
      find-method
      .getModifiers
      Modifier/isStatic))

(defn read-field
  "Returns the value of a field, static if x is a class, or on the
  passed instance otherwise. All fields, private, protected or public,
  are accessible."
  [x name]
  (if (class? x)
    (-> (doto (.. x (getDeclaredField (str name)))
              (.setAccessible true))
        (.get nil))
    (-> (doto (.. x getClass (getDeclaredField (str name)))
              (.setAccessible true))
        (.get x))))

(defn method
  "Returns a method by reflection."
  [klass nme parameter-types]
  (let [klass (if (class? klass) klass (resolve (-> klass name symbol)))
        m (-> (doto (.getDeclaredMethod klass (name nme)
                                        (into-array Class parameter-types))
                    (.setAccessible true)))]
    (fn [target & args]
      (.invoke m target (to-array args)))))

(defn static-method
  "Returns a static method by reflection."
  [class name parameter-types]
  (let [m (method class name parameter-types)]
    (partial m nil)))

