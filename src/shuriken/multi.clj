(ns shuriken.multi)

(defn call-multi [multi dispatch-val & args]
  (apply (-> multi methods (get dispatch-val))
         args))
