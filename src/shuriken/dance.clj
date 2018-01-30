(ns shuriken.dance
  "### Advanced tree walking"
  (:use clojure.pprint)
  (:require [shuriken.debug :refer [debug-print]]
            [shuriken.fn :refer [arities max-arity fake-arity]]
            [shuriken.associative :refer [merge-with-plan]]
            [shuriken.weaving :refer [and| ->| apply| context| warp|
                                      not| arity-comp]]))

(defn- wrap? [k form]
  (boolean
    (and (vector? form)
         (some-> form first #{k}))))

(defn- wrap [k form]
  (if (wrap? k form)
    form
    [k form]))

(defn- unwrap [k form]
  (if (wrap? k form)
    (second form)
    form))

(def ^:private no-walk? (partial wrap? ::no-walk))
(def ^:private no-walk  (partial wrap  ::no-walk))
(def ^:private unwalk   (partial unwrap ::no-walk))

(defn- adapt-dance-fns [dance]
  (->> dance
       (map (fn [[k f]]
              (if (#{:walk? :pre? :pre :post? :post :before :after
                     :before-all :after-all}
                     k)
                [k (context| f)]
                [k f])))
       (into {})))

(def ^:private dance-identity (context| identity))

(defn step [form {:keys [initial-acc initial-context context wrap
                         inner outer scoped]}]
  (let [[result context]
        (reduce (fn [[acc prev-ctx] subform]
                  (let [original-scope (select-keys prev-ctx scoped)
                        [result new-ctx] (inner subform prev-ctx)
                        new-ctx (merge new-ctx original-scope)]
                    [(conj acc result) new-ctx]))
                [initial-acc initial-context]
                form)]
    (outer (wrap result) context)))

(defn step-indexed [form {:keys [initial-acc initial-context context wrap
                                 inner outer scoped]}]
  (let [indexes (if (record? form)
               (map key form)
               (range))
        [result context]
        (reduce (fn [[acc prev-ctx] [i subform]]
                  (let [ctx (assoc prev-ctx :index i)
                        original-scope (select-keys prev-ctx scoped)
                        [result new-ctx] (inner subform ctx)
                        new-ctx (assoc new-ctx :index i)
                        new-ctx (merge new-ctx original-scope)]
                    [(conj acc result) new-ctx]))
                [initial-acc initial-context]
                (zipmap indexes form))]
    (outer (wrap result) context)))

(defn- context-walk [step scoped inner outer context form]
  (let [marche (fn [& {:as opts}]
                 (step form (merge {:initial-acc []
                                    :initial-context context
                                    :wrap identity
                                    :inner inner :outer outer
                                    :scoped scoped}
                                   opts)))]
    (cond
      (list? form)      (marche :wrap #(apply list %))
      (map-entry? form) (marche :wrap vec)
      (seq? form)       (doall (marche :wrap seq))
      (record? form)    (marche :initial-acc form)
      (coll? form)      (marche :wrap #(into (empty form) %))
      :else             (outer form context))))

(defn- chain-dance-fns [f & more-fns]
  (->> (map apply| more-fns)
       (concat [f])
       (apply ->|)))

(defn- reverse-chain-dance-fns [& fns]
  (apply chain-dance-fns (reverse fns)))

;; TODO: rewrite warp with interleave
(defmacro ^:private contextualize [name f]
  `(def ^:private ~name
     (warp| ~f (fn
                 ([g# [form# ctx#]] (g# form# ctx#))
                 ([g# form# ctx#]   (g# form# ctx#))))))

(contextualize ctx-comp arity-comp)
(contextualize ctx->|   ->|)

(defn ctx-and|
  [& fns]
  (fake-arity
    (->> fns (mapcat arities) distinct sort)
    #(loop [[f & more] fns]
       (let [[result ctx] (apply f %&)]
         (if (and result (seq more))
           (recur more)
           [result ctx])))))

(defn- reverse-ctx-and| [& fns]
  (apply ctx-and| (reverse fns)))

(def ^:private dance-merge-plan
  {:before ctx->|
   :pre?  ctx-and|         :pre   ctx->|
   :walk? ctx-and|
   :post? reverse-ctx-and| :post  ctx-comp
   :after ctx-comp
   :context merge
   :scoped  concat
   ;; :debug, :return-context, and :step are merged normally
   ;; (rightmost preference).
   })

(def empty-dance
  (adapt-dance-fns
    {:walk?      any?
     :pre?       any?     :pre            identity
     :post?      any?     :post           identity
     :before     identity :after          identity
     :before-all identity :after-all      identity
     :context    nil      :return-context false
     :scoped     #{}      :step           step
     :debug      false}))

(defn merge-dances
  "Merges multiples dances together. A dance is a hash of arguments
  to [[dance]]."
  [& dances]
  (merge empty-dance
         (apply merge-with-plan dance-merge-plan
                (map adapt-dance-fns dances))))

(def depth-dance
  "A dance that keeps track of the "
  {:scoped [:depth]
   :before (fn [x ctx] [x (update ctx :depth (fnil inc -1))])})

(def indexed-dance
  {:step  step-indexed
   :after (fn [form ctx]
            [form (dissoc ctx :index)])})

(def path-dance
  (merge-dances
    indexed-dance
    {:scoped [:path]
     :before (fn [form ctx]
              (let [conj-path (fn [ctx k]
                                (update ctx :path
                                        (comp vec concat)
                                        (if-let [i (get ctx k)]
                                          [i] [])))
                    new-ctx (cond
                              (map-entry? form)
                              (assoc ctx :map-index (key form))
                              
                              (contains? ctx :map-index)
                              (case (:index ctx)
                                0 ctx
                                1 (-> (conj-path ctx :map-index)
                                      (dissoc :map-index)))
                              
                              :else (conj-path ctx :index))]
                [form new-ctx]))}))

(def leaf-collecting-dance
  (merge-dances
    path-dance
    {:pre? (not| coll?)
     :pre (fn [form ctx]
           (let [new-ctx (update ctx :leafs assoc (:path ctx) form)]
             [form new-ctx]))
     :after-all (fn [form ctx]
                  [(:leafs ctx) ctx])
     :debug true}))

(def ^:dynamic *default-dance*
  {})

(defmacro ^:private add-debugs [debugs let-statement]
  (let [debugs (resolve (or (and (symbol? debugs) debugs)
                            debugs))]
    (->> (update (vec let-statement) 1
                 (fn [bindings]
                   (->> bindings
                        (partition 2)
                        (map (fn [[k v]]
                               (if-let [d (debugs k)]
                                 [['prev-ctx 'ctx]
                                  [k v]
                                  ['_ d]]
                                 [[k v]])))
                        (apply concat)
                        (apply concat)
                        vec)))
         (apply list))))

(def ^:private dance*-debugs
  '{[should-walk ctx]
    (when debug
      (let [tabs (str (apply str (repeat depth "  "))
                      "- ")]
        (if should-walk
          (debug-print (str tabs "Walking     ") form)
          (debug-print (str tabs "Not walking ") form)))
      (when debug-context
        (debug-print   (str tabs "Context     ") ctx)))
    
    [pre-result ctx] 
    (when debug
      (if should-pre
        (debug-print   (str tabs "Pre         ") pre-result)
        (debug-print   (str tabs "No pre      ") pre-result))
      (when (and debug-context (not= ctx prev-ctx))
        (debug-print   (str tabs "New context ") ctx)))
    
    unwalked
    (when (and debug unwalked)
      (debug-print     (str tabs "Unwalked    ") unwalked))
    
    [result ctx]
    (when debug
      (if should-post
        (debug-print   (str tabs "Post        ") result)
        (debug-print   (str tabs "No post     ") result))
      (when (and debug-context (not= ctx prev-ctx))
        (debug-print   (str tabs "New context ") ctx)))})

;; Variable names in this function matter for the debugging from above.
(defn- dance*
  [form {:keys [walk?
                pre?    pre
                post?   post
                before  after                
                context return-context
                debug   debug-context
                scoped  step]
         :as opts}]
  (add-debugs
    dance*-debugs
    (let [[form ctx]        (before form context)
          depth             (get ctx :depth 0)
          tabs              (apply str (repeat (inc depth) "  "))
          [should-walk ctx] (walk? form ctx)
          [should-pre  ctx] (pre? form ctx)
          [pre-result  ctx] (if should-pre (pre form ctx) [form ctx])
          unwalked          (when (no-walk? pre-result) (unwalk pre-result))
          opts              (assoc opts :context ctx)
          [out ctx]         (if (and should-walk (not unwalked))
                              (context-walk
                                step
                                scoped
                                (fn [form ctx]
                                  (dance* form (assoc opts :context ctx)))
                                dance-identity
                                ctx
                                pre-result)
                              (if unwalked [unwalked ctx] [pre-result ctx]))
          [should-post ctx] (post? out ctx)
          [posted ctx]      (if should-post (post out ctx) [out ctx])
          [result ctx]      (after posted ctx)]
      [result ctx])))

;; TODO: support deepmerge
;; - the way contexts are merged.
;; - :scoped option
(defn dance
  "A finely tunable version of clojure.walk with enhancements.
  
  For a demo, just run
  ```clojure
  (dance
    '(+ 1 (- 2 (* 3 (/ 4 5) (dec 3))))
    :walk?          #(and (seq? %) (-> % first (not= 'dec)))
    :pre?           #(and (seq? %) (-> % first (not= '/)))
    :pre            vec
    :post?          number?
    :post           (fn [x ctx] [(inc x) (update ctx :cnt inc)])
    :context        {:cnt 0}
    :return-context true
    :debug          true
    ;:debug-context  false
    )
  ```
  
  #### Dance fns
  
  See https://en.wikipedia.org/wiki/Tree_traversal
  
  Namely `walk?`, `pre?`, `pre`, `post?`, `post`, `before`, `after`,
  `before-all` and `after-all`.
  
  `pre?` and `post?` respectively condition `pre` and `post` while the
  walk of the substructure itself occurs in between and is conditioned
  by `walk?`. `before`and `after` are respectively called before and
  after any node is processed while @before-all` and `after-all` are
  called once, at the beginning and the end of the walk.
  
  Traversal appears to occur in pre-order for `before`, `walk?` `pre?`
  and `pre`, but in post-order for `post?`, `post` and `after`.
  
  Note that nodes that will not be walked might still be processed by
  `pre` and `post`.
  
  #### Context
  
  Dance fns can have 1 argument (the node at hand) or 2, in order to
  receive an optional, latent context. If so, they must return a
  vector like `[result context]` where `result` can be a processed
  substructure (`pre`, `post`, ...) or the return value of a predicate
  (`pre?`, `post?`, ...).
  
  This context is passed walking down the structure from node to
  subnodes then back up to the initial root in a depth-first manner.
  
  By default, it is global to the walk and is not scoped by the depth
  at which it being accessed. In other words, the context is passed
  from siblings to siblings in the order of the walk and modifications
  done to it when walking a node can be seen when walking the sister
  nodes, their children and eventually their ancestors.
  
  However the `:scoped` option can be used to specify which keys in
  the context should be scoped by the walk and not thread through the
  whole traversal. More specifically parents pass the context to their
  children and changes made by the children are not visibile by their
  ancestors or siblings.
  
  Note that variadic arguments functions count as single-argument to
  accomodate for functions like `constantly`, and thus cannot receive
  a context.
  
  #### Merging dances
  
  Multiple dances can be passed to `dance`, including a varargs dance:
  
  ```clojure
  (dance collection
    first-dance
    second-dance
    :before third-dance-before
    :after  third-dance-after)
  ```
  
  Options in these dance will be merged in a smart way according to
  this plan:
  
  ```
  - before, pre : composed from left (oldest) to right (newest)
  - after, post : composed from right to left
  - pre?, walk? : composed like `and`, from left to right
  - post?       : composed like `and`, but from right to left.
  - context     : composed with `merge`
  - scoped      : composed with `concat`
  ```
  
  `:debug`, `:return-context`, and `:step` are merged normally, i.e.
  via right-most preference.
  
  #### Additional options
  
  - `context`       : The initial context (defaults to `{}`)
  - `return-context`: to return a vector like `[result context]`
                      (`false`).
  - `debug`         : To print a detailed trace of the walk (`false`).
  - `depth`         : The intial depth. Determines tabulation (`0`)
                      when debugging.
  
  Any option passed to `dance` is optional including the dance fns."
  [form & args]
  (let [dances     (take-while map? args)
        args-dance (->> args 
                        (drop-while map?)
                        (partition 2)
                        (map vec)
                        (into {}))
        dances (concat dances [args-dance])
        opts-dance (apply merge-dances *default-dance* dances)
        opts-dance (if (:debug opts-dance)
                     (merge-dances depth-dance opts-dance)
                     opts-dance)
        debug-context (if (contains? opts-dance :debug-context)
                        (:debug-context args-dance)
                        (or (:return-context args-dance)
                            (contains? args-dance :context)
                            (some #(= % 2)
                                  (->> args-dance
                                       ((juxt :walk? :pre? :pre :post? :post
                                              :before :after))
                                       (remove nil?)
                                       (mapcat arities)))))
        {:keys [before-all after-all]} opts-dance
        [form ctx] (before-all form (:context opts-dance))
        [danced ctx] (dance* form (-> opts-dance
                                      (dissoc :before-all :after-all)
                                      (assoc
                                        :context       ctx
                                        :debug-context debug-context)))
        [result ctx] (after-all danced ctx)]
    (if (:return-context opts-dance)
      [result ctx]
      result)))
