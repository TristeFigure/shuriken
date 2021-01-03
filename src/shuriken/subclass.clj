(ns shuriken.subclass
  (:use clojure.pprint shuriken.macro)
  (:require [shuriken.monkey-patch :refer [deep-copy-class defrosting-freezing
                                           save-class save-classes
                                           class-exists?]]
            [shuriken.reflection :refer [find-class find-method
                                         read-field write-field]]
            [shuriken.exception :refer [silence]]))

(defmacro defsubinterface [parent name & sigs]
  (let [tag (fn [x] (or (:tag (meta x)) Object))
        psig (fn [[name [& args]]]
               (vector name (vec (map tag args)) (tag name) (map meta args)))
        cname (with-meta (symbol (str (namespace-munge *ns*) "." name)) (meta name))]
    `(let [] ;; TODO: do ?
       (gen-interface :name ~cname :methods ~(vec (map psig sigs))
                      ;; the only diff with definterface is the following line
                      ;; (and the added 'parent' arg)
                      :extends ~[(-> parent resolve ((memfn ^Class getName)) symbol)])
       (import ~cname))))


(defsubinterface clojure.lang.IType ISubclassType)

(def ^:private validate-fields          @#'clojure.core/validate-fields)
(def ^:private parse-opts+specs         @#'clojure.core/parse-opts+specs)
(def ^:private emit-deftype*            @#'clojure.core/emit-deftype*)
(def ^:private build-positional-factory @#'clojure.core/build-positional-factory)

(def ^:no-doc ^:dynamic *deftype-parent*
  Object)

(def compiler-with-custom-deftype-parent
  (let [{compiler-copy          'shuriken.subclass.CompilerWithCustomParentDeftype
         new-instance-expr-copy 'shuriken.subclass.CompilerWithCustomParentDeftype$NewInstanceExpr
         :as ct-classes}
        (deep-copy-class clojure.lang.Compiler
                         'shuriken.subclass.CompilerWithCustomParentDeftype)
        _ (defrosting-freezing new-instance-expr-copy
            (let [m (find-method
                      [new-instance-expr-copy "build"
                       [clojure.lang.IPersistentVector ; interfaceSyms
                        clojure.lang.IPersistentVector ; fieldSyms
                        clojure.lang.Symbol            ; thisSym
                        java.lang.String               ; tagName
                        clojure.lang.Symbol            ; className
                        clojure.lang.Symbol            ; typeTag
                        clojure.lang.ISeq              ; methodForms
                        java.lang.Object               ; frm
                        clojure.lang.IPersistentMap]]) ; opts
                  substitute-parent
                  "{
                      $1 = clojure.lang.Var.intern(
                            clojure.lang.Symbol.intern(\"shuriken.subclass\"),
                            clojure.lang.Symbol.intern(\"*deftype-parent*\")
                          ).deref();

                      $_ = $proceed($$);
                  }"
                  expr-editor
                  (proxy [javassist.expr.ExprEditor] []
                    (edit [method-call]
                          (when (instance? javassist.expr.MethodCall
                                           method-call)
                            (if (#{"gatherMethods" "slashname"}
                                   (.getMethodName method-call))
                              (.replace method-call substitute-parent)))))]
              (.instrument m expr-editor)))
        java-classes (save-classes (vals ct-classes))]
    (get java-classes 'shuriken.subclass.CompilerWithCustomParentDeftype)))

(println "compiler-with-custom-deftype-parent"
         compiler-with-custom-deftype-parent)

(defn- emit-defsubclass*
  [tagname cname fields interfaces methods opts]
  (let [classname (with-meta (symbol (str (namespace-munge *ns*) "." cname)) (meta cname))
        ;; only diff with emit-deftype* is the following line
        interfaces (conj interfaces 'shuriken.subclass.ISubclassType)]
    `(deftype* ~(symbol (name (ns-name *ns*)) (name tagname)) ~classname ~fields
       :implements ~interfaces
       ~@(mapcat identity opts)
       ~@methods)))

(defmacro defsubclass [parent name fields & opts+specs]
  (validate-fields fields name)
  (let [gname name
        [interfaces methods opts] (parse-opts+specs opts+specs)
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." gname))
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        [field-args over] (split-at 20 fields)]
    `(let []
       ~(emit-defsubclass* name gname (vec hinted-fields) (vec interfaces) methods opts)
       (import ~classname)
       ~(build-positional-factory gname classname fields)
       ~classname)))

; (pprint (macroexpand '(defsubclass clojure.lang.LispReader TotoReader [])))

;; Ok there are some problems with this approach since we can't pass the
;; type superclass. It is always Object.
;; Check out this method from clojure.core.compiler
;; defsubinterface works though

; build(
;       (IPersistentVector)RT.get(opts,implementsKey,PersistentVector.EMPTY),
;       fields,
;       null,
;       tagname,
;       classname,
;       (Symbol) RT.get(opts,RT.TAG_KEY),rform, frm, opts
;   );


