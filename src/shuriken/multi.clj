(ns shuriken.multi)

;; TODO: document here and in the README
(defn method [multi dispatch-val]
  (-> multi methods (get dispatch-val)))

(defn call-method [& args]
  (let [[method args] (if (-> args first class (isa? clojure.lang.MultiFn))
                        (let [[multi dispatch-val & args] args]
                          [(method multi dispatch-val) args])
                        [(first args) (rest args)])]
    (apply method args)))

(defn super-method [multi dispatch-val]
  (let [default (.defaultDispatchVal multi)
        ps (or (parents dispatch-val) #{default})
        _ (assert (not (> (count ps) 1))
                  (format "Multiple parents found for dispatch val %s: %s"
                          dispatch-val ps))
        p (first ps)]
    (if-let [m (method multi p)]
      m
      (if (= p default)
        (throw (IllegalArgumentException.
                 (format (str "No super-method in multimethod '%s' for "
                              "dispatch value %s")

                         (pr-str dispatch-val))))
        (super-method multi p)))))

;; ===== Augment: compose with the previous implementation
(def augment-composers
  (atom {}))

(defn augmentable-multi [multi composer]
  (swap! augment-composers assoc multi composer))

;; TODO: support multiple bodies. Pff.
(defmacro augment-method [multi dispatch-val args & body]
  (let [original-name (gensym (str "augmented-original"
                                   (when (symbol? multi) multi)
                                   "-"))]
    `(let [m# ~multi
           d# ~dispatch-val
           composer# (get @augment-composers m#)]
       (do (def ~original-name (-> m# methods (get d#)))
           (defmethod m# d# [& args#]
             (apply (composer# ~original-name (fn ~args ~@body))
                    args#))))))

;; ===== Extend: compose with the super implementation
(def extend-composers
  (atom {}))

(defn extendable-multi [multi composer]
  (swap! extend-composers assoc multi composer))

;; TODO: support multiple bodies. Pff.
(defmacro extend-method [multi dispatch-val args & body]
  `(let [m# ~multi
         d# ~dispatch-val
         composer# (get @extend-composers m#)]
     (do (defmethod m# d# [& args#]
           (apply (composer# (super-method m# d#) (fn ~args ~@body))
                  args#)))))
