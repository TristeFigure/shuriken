(require 'shuriken.namespace)

(shuriken.namespace/once-ns
  (ns shuriken.monkey-patches.syntax-quote
    (:require [com.palletops.ns-reload :as nsdeps]
              [shuriken.namespace :refer [with-ns]]
              [clojure.tools.reader])
    (:import RedefineClassAgent))

  ;; Step 2: Tag forms expanded with syntax-quote in clojure.tools.reader
  (with-ns 'clojure.tools.reader
    (def ^:private original-macros macros)

    (defn- read-syntax-quoted
      [rdr backquote opts pending-forms]
      (binding [gensym-env {}]
        (let [ret (-> (read* rdr false nil opts pending-forms)
                      syntax-quote*
                      (vary-meta update-in [::syntax-quotes] (fnil inc 0))
                      #_(list 'clojure.tools.reader/syntax-quoted)
                      )]
          (println "META:" (meta ret))
          ret)))

    (defn- macros [ch]
      (case ch
        \` read-syntax-quoted
        (original-macros ch)))

    (defn ^:private read*
      ([reader eof-error? sentinel opts pending-forms]
       (read* reader eof-error? sentinel nil opts pending-forms))
      ([reader eof-error? sentinel return-on opts pending-forms]
       (when (= :unknown *read-eval*)
         (err/reader-error "Reading disallowed - *read-eval* bound to :unknown"))
       (try
         (loop []
           (log-source reader
                       (if (seq pending-forms)
                         (.remove ^List pending-forms 0)
                         (let [ch (read-char reader)]
                           (cond
                             (whitespace? ch) (recur)
                             (nil? ch) (if eof-error? (err/throw-eof-error reader nil) sentinel)
                             (= ch return-on) READ_FINISHED
                             (number-literal? reader ch) (read-number reader ch)
                             :else (let [f (macros ch)]
                                     (if f
                                       (let [res (f reader ch opts pending-forms)]
                                         (if (identical? res reader)
                                           (recur)
                                           res))
                                       (read-symbol reader ch))))))))
         (catch Exception e
           (if (ex-info? e)
             (let [d (ex-data e)]
               (if (= :reader-exception (:type d))
                 (throw e)
                 (throw (ex-info (.getMessage e)
                                 (merge {:type :reader-exception}
                                        d
                                        (if (indexing-reader? reader)
                                          {:line   (get-line-number reader)
                                           :column (get-column-number reader)
                                           :file   (get-file-name reader)}))
                                 e))))
             (throw (ex-info (.getMessage e)
                             (merge {:type :reader-exception}
                                    (if (indexing-reader? reader)
                                      {:line   (get-line-number reader)
                                       :column (get-column-number reader)
                                       :file   (get-file-name reader)}))
                             e)))))))

    (def original-read read)
    (defn read [& args]
      (let [result (apply original-read args)]
        result)))

  ;; Step 1: substitute Clojure's native reader with that from
  ;; clojure.tools.reader and setup syntax-quote macro
  (with-ns 'clojure.core
    (def read        clojure.tools.reader/read)
    (def read-string clojure.tools.reader/read-string))

  ;; Also change the reader used by Clojure's Compiler to use
  ;; clojure.tools.reader/read
  (defn read-delegate [reader eof-is-error eof-value _is-recursive opts]
    (clojure.tools.reader/read
      (merge {:eofthrow eof-is-error
              :eof  eof-value}
             opts)
      reader))

  (let [class-name "clojure.lang.LispReader"
        class-pool (javassist.ClassPool/getDefault)
        ct-class   (.get class-pool class-name)]
    (.stopPruning ct-class true)
    (when (.isFrozen ct-class)
      (.defrost ct-class))
    (let [method (->> (.getMethods ct-class)
                      (filter #(and (= "read" (.getName %))
                                    (= (count (.getParameterTypes %))
                                       5)))
                      first)]
      (.setBody
        method
        (format "{
                  clojure.lang.IFn readFn = clojure.java.api.Clojure.var(
                    \"%s\", \"read-delegate\"
                  );
                  return readFn.applyTo(clojure.lang.RT.seq($args));
                }",
                (str *ns*)))
      (let [bytecode (.toBytecode ct-class)
            definition (java.lang.instrument.ClassDefinition.
                         (Class/forName class-name)
                         bytecode)]
        (RedefineClassAgent/redefineClasses
          (into-array java.lang.instrument.ClassDefinition[definition])))))

  ;; Step 3: Add (syntax-quoted x) -> `(x) translation into clojure.pprint
  ;;         and translate (unquote-splicing args) to ~@args as well.
  (require 'clojure.pprint)
  (with-ns 'clojure.pprint
    (def ^:private original-pprint-reader-macro pprint-reader-macro)

    (println "--------->" pprint-reader-macro)
    (defn- pprint-reader-macro [alis]
      (or (original-pprint-reader-macro alis)
          (when (and (= (first alis) 'clojure.tools.reader/syntax-quoted)
                     (= 2 (count alis)))
            (.write ^java.io.Writer *out* "`")
            (write-out (eval (second alis)))
            true)))

    (defn- pprint-list [alis]
      (if-not (pprint-reader-macro alis)
        (pprint-simple-list alis)))

    (use-method simple-dispatch clojure.lang.ISeq pprint-list))

  ;; Step 5: reload clojure.core from dependent namespaces
  (doseq [ns-sym (concat (nsdeps/dependent-namespaces 'clojure.core)
                         (when (find-ns 'user)
                           '[user]))]
    (with-ns ns-sym
      (require '[clojure.core :reload true]))))
