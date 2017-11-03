(require 'shuriken.namespace)

(shuriken.namespace/once-ns
  (ns shuriken.monkey-patches.macroexpand
    (:require [clojure.java.io :as io] )
    (:import RedefineClassAgent))

  (defn original-macroexpand1 [^Object x]
    (if (instance? clojure.lang.ISeq)
      (let [form x
            ^Object op (first form)]
        (if (clojure.lang.Compiler/isSpecial op)
          x
          (let [v (clojure.lang.Compiler/isMacro op)]
            (if-not (nil? v)
              (do (clojure.lang.Compiler/checkSpecs v form)
                  (try
                    (let [args (clojure.lang.RT/cons
                                 (.get clojure.lang.Compiler/LOCAL_ENV)
                                 (.next form))]
                      (apply v args))
                    (catch clojure.lang.ArityException e
                      (throw (ArityException. (- (.actual e) 2) (.name e))))))
              (if (instance? clojure.lang.Symbol op)
                (let [sym op
                      sname (.name sym)]
                  (if (-> sym .name (.charAt 0) (= "."))
                    (do
                      (when (< (clojure.lang.RT/length form) 2)
                        (throw (IllegalArgumentException.
                                 (str "Malformed member expression, expecting "
                                      "(.member target ...)"))))
                      (let [meth (clojure.lang.Symbol/intern
                                   (.substring sname 1))
                            target (clojure.lang.RT/second form)
                            target (if (clojure.lang.Compiler$HostExpr/maybeClass
                                         target false)
                                     (-> (clojure.lang.RT/list
                                           clojure.lang.Compiler/IDENTITY target)
                                         (.withMeta
                                           (clojure.lang.RT/map
                                             clojure.lang.RT/TAG_KEY
                                             clojure.lang.Compiler/CLASS)))
                                     target)]
                        (.preserveTag
                          form
                          (clojure.lang.RT/listStar
                            clojure.lang.Compiler/DOT
                            target
                            meth
                            (.next form)))))
                    (if (clojure.lang.Compiler/nameStaticMember sym)
                      (let [target (clojure.lang.Symbol/intern (.ns sym))
                            c (clojure.lang.Compiler$HostExpr/maybeClass
                                target false)]
                        (if-not (nil? c)
                          (let [meth (clojure.lang.Symbol/intern (.name sym))]
                            (.preserveTag
                              form
                              (clojure.lang.RT/listStar
                                clojure.lang.Compiler/DOT
                                target
                                meth
                                (.next form))))
                          x))
                      (let [idx (.lastIndexOf sname ".")]
                        (if (= idx (dec (.length sname)))
                          (clojure.lang.RT/listStar
                            clojure.lang.Compiler/NEW
                            (clojure.lang.Symbol/intern
                              (.substring sname 0 idx)
                              (.next form)))
                          x)))))))))))))

  (defn macroexpand1-delegate [& args]
    (apply original-macroexpand1 args))

  (let [class-name "clojure.lang.Compiler"
        class-pool (javassist.ClassPool/getDefault)
        ct-class   (.get class-pool class-name)]
    (.stopPruning ct-class true)
    (when (.isFrozen ct-class)
      (.defrost ct-class))
    (let [method (.getDeclaredMethod ct-class "macroexpand1")]
      (.setBody
        method
        (format
          "{
            clojure.lang.IFn expandFn = clojure.java.api.Clojure.var(
              \"%s\", \"macroexpand1-delegate\"
            );

            return expandFn.applyTo(clojure.lang.RT.seq($args));
          }",
          (str *ns*)))
      (let [bytecode (.toBytecode ct-class)
            definition (java.lang.instrument.ClassDefinition.
                         (Class/forName class-name)
                         bytecode)]
        (RedefineClassAgent/redefineClasses
          (into-array java.lang.instrument.ClassDefinition[definition]))))))
