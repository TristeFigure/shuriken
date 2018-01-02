(ns shuriken.dance
  (:use clojure.pprint
        shuriken.debug)
  (:require [shuriken.debug :refer [debug-print]]
            [shuriken.fn :refer [arity]]
            [shuriken.associative :refer [merge-with-plan]]
            [shuriken.weaving :refer [and| ->| apply| context|]]))

;; TODO: expose?
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

(def ^:private context?  (partial wrap? ::context))
(def ^:private context   (partial wrap  ::context))
(def ^:private uncontext (partial unwrap ::context))

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

(defn adapt-dance-fns [dance]
  (->> dance
       (map (fn [[k f]]
              (if (#{:walk? :pre? :pre :post? :post :before :after} k)
                [k (context| f)]
                [k f])))
       (into {})))

(def dance-identity (context| identity))

(defn- step [form {:keys [initial-acc initial-context context wrap inner outer
                         keys]
                  :or {initial-acc     []
                       initial-context {}
                       wrap            identity}}]
  (let [[result context]
        (reduce (fn [[acc prev-ctx] [k form]]
                        (let [[result new-ctx] (inner form prev-ctx k)]
                          [(conj acc result) new-ctx]))
                      [initial-acc initial-context]
                      (zipmap keys form))]
    (outer (wrap result) context)))

(defn context-walk [inner outer context form]
  (let [marche (fn [& {:as opts}]
                 (step form (merge {:inner inner :outer outer
                                    :initial-context context
                                    :keys (range)}
                                   opts)))]
    (cond
      (list? form)      (marche :wrap #(apply list %))
      (map-entry? form) (marche :wrap vec :keys [(key form)])
      (seq? form)       (doall (marche :wrap seq))
      (record? form)    (marche :initial-acc form :keys (map key form))
      (coll? form)      (marche :wrap #(into (empty form) %))
      :else             (outer form context))))

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

;; Variable name in this function matter. Check out dance*-debugs and add-debugs
;; to find out how.
(defn- dance*
  [form {:keys [walk?
                pre?    pre
                post?   post
                before  after                
                context return-context
                debug   debug-context]
         :as opts}]
  (add-debugs
    dance*-debugs
    (let [[form ctx]        (before form context)
          depth             (:depth ctx)
          tabs              (apply str (repeat (inc depth) "  "))
          [should-walk ctx] (walk? form ctx)
          [should-pre  ctx] (pre? form ctx)
          [pre-result  ctx] (if should-pre (pre form ctx) [form ctx])
          unwalked          (when (no-walk? pre-result) (unwalk pre-result))
          opts              (assoc opts :context ctx)
          [out ctx]         (if (and should-walk (not unwalked))
                              (context-walk
                                (fn [form ctx k]
                                  (dance* form (assoc opts :context ctx)))
                                dance-identity
                                ctx
                                pre-result)
                              (if unwalked [unwalked ctx] [pre-result ctx]))
          [should-post ctx] (post? out ctx)
          [result ctx]      (if should-post (post out ctx) [out ctx])
          [aftered ctx]     (after result ctx)]
      [aftered ctx])))

(def empty-dance
  {:walk?   any?
   :pre?    any?     :pre   identity
   :post?   any?     :post  identity
   :before  identity :after identity
   :context nil      :return-context false
   :debug   false})

(defn- chain-dance-fns [f & more-fns]
  (->> (map apply| more-fns)
       (concat [f])
       (apply ->|)))

(defn- reverse-chain-dance-fns [& fns]
  (let [fns (reverse fns)
        [f & more-fns] fns]
    (->> (map apply| more-fns)
         (concat [f])
         (apply ->|))))

(def dance-merge-plan
  {:walk? and|
   :pre?  and|                       :pre   chain-dance-fns
   :post? #(apply and| (reverse %&)) :post  reverse-chain-dance-fns
   :before chain-dance-fns           :after reverse-chain-dance-fns})

(defn merge-dances [& dances]
  (merge empty-dance
         (apply merge-with-plan
                dance-merge-plan
                (map adapt-dance-fns dances))))

(def depth-dance
  {:before (fn [x ctx] [x (update ctx :depth (fnil inc -1))])
   :after  (fn [x ctx] [x (if (zero? (:depth ctx))
                            (dissoc ctx :depth)
                            (update ctx :depth dec))])
   :depth 0})

(def default-dance depth-dance)

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
  
  See https://en.wikipedia.org/wiki/Tree_traversal for a
  
  Namely `walk?`, `pre?`, `pre`, `post?`, `post` `before` and `after`
  
  `pre?` and `post?` condition `pre` and `post` respectively while the
  walk of the substructure itself occur in between and is conditioned
  by `walk?`.
  
  Traversal appears to occur in pre-order for `walk?` `pre?` and
  `pre`, but in post-order for `post?` and `post`.
  
  Note that nodes that will not be walked might still be processed by
  `pre` and `post`.
  
  #### Context
  
  Dance fns can have 1, 2 or variadic arguments in order to receive an
  optional, latent context. If so, they must return a vector like
  `[result context]`.
  
  This context is passed walking down the structure from node to
  subnodes then up to the initial root in a depth-first manner.
  
  It is global to the walk and is not scoped by the depth at which it
  is accessed. In other words, the context is passed from siblings to
  siblings in the order of the walk and modifications done to it when
  walking a node can be seen when walking the sister nodes and their
  children.
  
  #### Additional options
  
  - `context`       : The initial context (defaults to `{}`)
  - `return-context`: to return a vector like `[result context]`
                      (`false`).
  - `debug`         : To print a detailed trace of the walk (`false`).
  - `depth`         : The intial depth. Determines tabulation (`0`).
  
  Any option passed to `dance` is optional including the dance fns
  themselves."
  [form & args]
  (let [{:keys [walk? pre? pre post? post before after
                debug context return-context debug-context]
           :as raw-opts}
        (if (and (= 1 (count args))
                 (-> args first map?))
          args
          (->> args (partition 2) (map vec) (into {})))
        debug-context (if (contains? raw-opts :debug-context)
                        debug-context
                        (or return-context
                            (contains? raw-opts :context)
                            (some #(> % 1)
                                  (map arity [walk? pre? pre post? post]))))
        opts (->> {:walk?   walk?
                   :pre?    pre?    :pre   pre
                   :post?   post?   :post  post
                   :before  before  :after after                   
                   :context context :return-context return-context
                   :debug   debug   :debug-context  debug-context}
                  (remove (fn [[k v]] (nil? v)))
                  (into {})
                  (merge-dances default-dance)
                  (map (fn [[k f]]
                         (if (#{:walk? :pre? :pre :post? :post} k)
                           [k (context| f)]
                           [k f])))
                  (into {}))
        [result ctx] (dance* form opts)]
    (if return-context
      [result ctx]
      result)))
