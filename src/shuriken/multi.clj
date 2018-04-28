(ns shuriken.multi)

(defn call-method [multi dispatch-val & args]
  (apply (-> multi methods (get dispatch-val))
         args))

(defn super-method [multi dispatch-val & args]
  (let [ps (or (parents dispatch-val) #{(.defaultDispatchVal multi)})
        _ (assert (not (> (count ps) 1))
                  (format "Multiple parents found for dispatch val %s: %s"
                          dispatch-val ps))
        p (first ps)]
    (apply call-method multi p args)))
