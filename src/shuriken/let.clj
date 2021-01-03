(ns shuriken.let)

(defmacro earlet [bindings & body]
  `(let ~(mapv macroexpand bindings)
     ~@body))

(defmacro lay [[sym expr & more-bindings] & body]
  (let [delay-sym (gensym (str "laid-" sym "-"))]
    `(let [~delay-sym (delay ~expr)]
       (symbol-macrolet [~sym (deref ~delay-sym)]
         ~@(if (empty? more-bindings)
             body
             `[(lay ~more-bindings ~@body)])))))

; See:
; - Handling :& in destructuring maps: https://gist.github.com/TristeFigure/822965f159eb5c578fb958cc301f070b
; - Clojure lazy let based on delays: https://gist.github.com/TristeFigure/09c1c6309b70ccdb5ae164606e3876e9
