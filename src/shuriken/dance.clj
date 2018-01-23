(ns shuriken.dance
  "### Advanced tree walking"
  (:use clojure.pprint)
  (:require [shuriken.debug :refer [debug-print]]
            [shuriken.fn :refer [arities]]
            [shuriken.associative :refer [merge-with-plan]]
            [shuriken.weaving :refer [and| ->| apply| context|]]))

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
              (if (#{:walk? :pre? :pre :post? :post :before :after} k)
                [k (context| f)]
                [k f])))
       (into {})))

(def ^:private dance-identity (context| identity))

(defn step [form {:keys [initial-acc initial-context context wrap
                          inner outer]}]
  (let [[result context]
        (reduce (fn [[acc prev-ctx] subform]
                        (let [[result new-ctx] (inner subform prev-ctx)]
                          [(conj acc result) new-ctx]))
                      [initial-acc initial-context]
                      form)]
    (outer (wrap result) context)))

(defn step-indexed [form {:keys [initial-acc initial-context context wrap
                                 inner outer]}]
  (let [indexes (if (record? form)
               (map key form)
               (range))
        [result context]
        (reduce (fn [[acc prev-ctx] [i subform]]
                  (let [ctx (assoc prev-ctx :index i)
                        [result new-ctx] (inner subform ctx)]
                    [(conj acc result) new-ctx]))
                [initial-acc initial-context]
                (zipmap indexes form))]
    (outer (wrap result) context)))

(defn- context-walk [step inner outer context form]
  (let [marche (fn [& {:as opts}]
                 (step form (merge {:initial-acc []
                                    :initial-context context
                                    :wrap identity
                                    :inner inner :outer outer}
                                   opts)))]
    (cond
      (list? form)      (marche :wrap #(apply list %))
      (map-entry? form) (marche :wrap vec)
      (seq? form)       (doall (marche :wrap seq))
      (record? form)    (marche :initial-acc form)
      (coll? form)      (marche :wrap #(into (empty form) %))
      :else             (outer form context))))

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
                step]
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
                                (fn [form ctx]
                                  (dance* form (assoc opts :context ctx)))
                                dance-identity
                                ctx
                                pre-result)
                              (if unwalked [unwalked ctx] [pre-result ctx]))
          _ (println "--->" [out ctx])
          [should-post ctx] (post? out ctx)
          _ (println "--->" [out ctx])
          [posted ctx]      (if should-post (post out ctx) [out ctx])
          [result ctx]      (after posted ctx)]
      [result ctx])))

(def ^:private empty-dance
  {:walk?   any?
   :pre?    any?     :pre   identity
   :post?   any?     :post  identity
   :before  identity :after identity
   :context nil      :return-context false
   :debug   false    :step  step})

(defn- chain-dance-fns [f & more-fns]
  (->> (map apply| more-fns)
       (concat [f])
       (apply ->|)))

(defn- reverse-chain-dance-fns [& fns]
  (apply chain-dance-fns (reverse fns)))

(def ^:private dance-merge-plan
  {:walk? and|
   :pre?  and|                       :pre   chain-dance-fns
   :post? (fn [& fns]
            (apply and| (reverse fns)))
   #_#(apply and| (reverse %&))
   :post  reverse-chain-dance-fns
   :before chain-dance-fns           :after reverse-chain-dance-fns})

(defn- prepare-dance [dance]
  (adapt-dance-fns
    (merge empty-dance dance)))

(def ff
  #(apply and| (reverse %&)))

(println "------>"
         ((and| (context| number?) (context| any?))
          :a))

(defn merge-dances
  "Merges multiples dances together. A dance is a hash of arguments
  to [[dance]]."
  [& dances]
  (apply merge-with-plan dance-merge-plan
         (map prepare-dance dances)))

(def depth-dance
  {:before (fn [x ctx] [x (update ctx :depth (fnil inc -1))])
   :after  (fn [x ctx] [x (if (zero? (:depth ctx))
                            (dissoc ctx :depth)
                            (update ctx :depth dec))])
   :depth 0})

(def indexed-dance
  {:step  step-indexed
   :after (fn [form ctx]
            [form (dissoc ctx :index)])
   ; :pre?  number?
   ; :post? number?
   })

(let [d1 {:pre? number? :post? number?}
      d2 (merge-dances {})
      md (merge-dances d2 d1)]
  (println "--> pre?"  ((:pre?  md) :a))
  (println "--> post?" ((:post? md) :a)))

(def pathed-dance
  (merge-dances indexed-dance))

(def ^:dynamic *default-dance*
  pathed-dance)

(throw (Exception. "fin"))

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
  
  Namely `walk?`, `pre?`, `pre`, `post?`, `post` `before` and `after`.
  
  `pre?` and `post?` respectively condition `pre` and `post` while the
  walk of the substructure itself occurs in between and is conditioned
  by `walk?`. `before`and `after` are respectively called before and
  after any node is processed.
  
  Traversal appears to occur in pre-order for `before`, `walk?` `pre?`
  and `pre`, but in post-order for `post?`, `post` and `after`.
  
  Note that nodes that will not be walked might still be processed by
  `pre` and `post`.
  
  #### Context
  
  Dance fns can have 1, 2 or variadic arguments in order to receive an
  optional, latent context. If so, they must return a vector like
  `[result context]`.
  
  This context is passed walking down the structure from node to
  subnodes then back up to the initial root in a depth-first manner.
  
  It is global to the walk and is not scoped by the depth at which it
  is accessed. In other words, the context is passed from siblings to
  siblings in the order of the walk and modifications done to it when
  walking a node can be seen when walking the sister nodes and their
  children.
  
  Variadic arguments functions count as single-argument to accomodate
  for functions like `constantly`, and thus cannot receive a context.
  
  #### Additional options
  
  - `context`       : The initial context (defaults to `{}`)
  - `return-context`: to return a vector like `[result context]`
                      (`false`).
  - `debug`         : To print a detailed trace of the walk (`false`).
  - `depth`         : The intial depth. Determines tabulation (`0`).
  
  Any option passed to `dance` is optional including the dance fns."
  [form & args]
  (let [{:keys [walk? pre? pre post? post before after
                debug context return-context debug-context step]
           :as raw-opts}
        (if (and (= 1 (count args))
                 (-> args first map?))
          (first args)
          (->> args (partition 2) (map vec) (into {})))
        debug-context (if (contains? raw-opts :debug-context)
                        debug-context
                        (or return-context
                            (contains? raw-opts :context)
                            (some #(> % 1)
                                  (->> [walk? pre? pre post? post]
                                       (remove nil?)
                                       (mapcat arities)))))
        args-dance (->> {:walk?   walk?
                         :pre?    pre?    :pre   pre
                         :post?   post?   :post  post
                         :before  before  :after after                   
                         :context context :return-context return-context
                         :debug   debug   :debug-context  debug-context
                         :step    step}
                        (remove (fn [[k v]] (nil? v)))
                        (into {}))
        opts-dance (merge-dances
                     (if debug depth-dance {})
                     *default-dance*
                     args-dance)
        _ (println "------------->"
                   ((:post? opts-dance) :a))
        [result ctx] (dance* form opts-dance)]
    (if return-context
      [result ctx]
      result)))
