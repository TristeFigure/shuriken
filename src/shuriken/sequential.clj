(ns shuriken.sequential)

(defn slice
  "Slice a seq using a delimiter predicate. There are two options:
  - :delimiter"
  [delimiter? coll & {:keys [include-delimiter include-empty]
                      :or {include-delimiter false
                           include-empty false}}]
  (let [not-delimiter? (complement delimiter?)
        [before-first-delim from-first-delim] (split-with not-delimiter? coll)
        result
        (loop [xs from-first-delim
               acc []]
          (if (empty? xs)
            acc
            (let [delim (first xs)
                  [after-delim next-slice] (split-with not-delimiter?
                                                       (rest xs))]
              (recur
                next-slice
                (if (and (not include-empty)
                         (empty? after-delim))
                  acc
                  (let [current-slice (case include-delimiter
                                        (nil false) after-delim
                                        :left (cons delim after-delim)
                                        :right (concat after-delim
                                                       (take 1 next-slice)))]
                    (conj acc current-slice)))))))]
    (seq (if (empty? before-first-delim)
           result
           (cons (if (= include-delimiter :right)
                   (concat before-first-delim (take 1 from-first-delim))
                   before-first-delim)
                 result)))))
