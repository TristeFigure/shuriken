(ns shuriken.reflection)

;; TODO: document in the README

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
  [class name parameter-types]
  (let [m (-> (doto (.getDeclaredMethod class (str name)
                                        (into-array Class parameter-types))
                (.setAccessible true)))]
    (fn [target & args]
      (.invoke m target (to-array args)))))

(defn static-method
  "Returns a static method by reflection."
  [class name parameter-types]
  (let [m (method class name parameter-types)]
    (partial m nil)))

