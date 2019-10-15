(ns shuriken.byte-buddy.dsl
  ;; IMPORTANT: take note of the excluded core vars
  (:use clojure.pprint shuriken.macro)
  (:refer-clojure :exclude [require name merge not load resolve])
  (:require [clojure.core :as clj]
            [clojure.set :as set]
            [camel-snake-kebab.core :refer [->kebab-case-symbol]]
            [shuriken.reflection :refer [instance-methods static-methods
                                         static-method?]]
            [shuriken.sequential :refer [separate form forcat takev maps]])
  (:import [java.lang.reflect
            Method
            Modifier]
           [net.bytebuddy.agent
            ByteBuddyAgent]
           [net.bytebuddy
            ByteBuddy
            NamingStrategy
            NamingStrategy$PrefixingRandom
            NamingStrategy$SuffixingRandom]
           [net.bytebuddy.implementation.auxiliary
            AuxiliaryType$NamingStrategy
            AuxiliaryType$NamingStrategy$SuffixingRandom]
           [net.bytebuddy.dynamic
            DynamicType$Builder
            DynamicType$Unloaded
            DynamicType$Loaded
            ClassFileLocator
            ClassFileLocator$Compound
            ClassFileLocator$ForClassLoader
            ClassFileLocator$Simple]
           [net.bytebuddy.dynamic.loading
            ClassLoadingStrategy
            ClassLoadingStrategy$Default
            ClassReloadingStrategy]
           [net.bytebuddy.asm
            MemberSubstitution]
           [net.bytebuddy.matcher
            ElementMatchers]
           [net.bytebuddy.description.type
            TypeDescription$ForLoadedType]
           [net.bytebuddy.pool
            TypePool
            TypePool$Default
            TypePool$Resolution]))

(def ^:private alphabet
  (map (comp symbol str char) (range (int \a) (inc (int \z)))))

(defn- emit-dsl [klass doc & {:keys [static]}]
  (let [class-name (-> klass .getName symbol)
        defs (for [ms (->> (if static
                             (static-methods klass)
                             (instance-methods klass))
                           (group-by (memfn ^Method getName))
                           vals
                           (map #(->> %
                                      (group-by (memfn ^Method getParameterCount))
                                      vals
                                      (map first))))
                   :let [method-name (-> ms first .getName)
                         fn-name     (-> method-name ->kebab-case-symbol)
                         method-sym  (symbol (str "." method-name))
                         get-arity   (fn [meth]
                                       (+ (.getParameterCount meth)
                                          (if (static-method? meth) 0 1)))
                         gen-fn-case (fn [arity subcases]
                                       (let [args (takev arity alphabet)
                                             [head-arg & rest-args] args
                                             [static-cases
                                              instance-cases]  (separate :static subcases)
                                             static-case       (first static-cases)
                                             instance-classes  (maps :class instance-cases)
                                             instance-impl     `(~method-sym
                                                                  ~@(when head-arg [head-arg])
                                                                  ~@rest-args)
                                             static-impl       (when static
                                                                 `(~(symbol (str (.getName (:class static-case))
                                                                                 "/" method-name))
                                                                     ~@args))]
                                         (assert (<= (count static-cases) 1))
                                         `(~args
                                            ~(cond
                                               (and (seq instance-cases) static-case)
                                               `(if (some #(instance? % ~head-arg)
                                                          instance-classes)
                                                  ~instance-impl
                                                  ~static-impl)

                                               static-case
                                               static-impl

                                               :else
                                               instance-impl))))
                         existing-cases (some-> fn-name clj/resolve meta ::cases)
                         new-cases (form [m ms]
                                     [(get-arity m) #{{:class class-name
                                                       :static static}}])
                         all-cases     (merge-with set/union existing-cases new-cases)
                         impl          (for [[arity subcases] all-cases]
                                         (gen-fn-case arity subcases))]]
               `(defn ~(with-meta fn-name `{::cases '~all-cases})
                  ~doc
                  ~@impl))]
    `(do ~@defs)))


(defmacro ^:private define-byte-buddy-dsl []
  (emit-dsl
    ByteBuddy
    "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/ByteBuddy.html"))

(defn buddy
  ([]                   (new ByteBuddy))
  ([class-file-version] (new ByteBuddy class-file-version)))

(define-byte-buddy-dsl)


(defmacro ^:private define-dynamic-type-builder-dsl []
  (emit-dsl
    DynamicType$Builder
    "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/dynamic/DynamicType.Builder.html"))

(define-dynamic-type-builder-dsl)


(defmacro ^:private define-member-substitution-dsl []
  (emit-dsl
    MemberSubstitution
    "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/asm/MemberSubstitution.html"))

(defn member-substitution [mode]
  (case mode
    :strict  (MemberSubstitution/strict)
    :relaxed (MemberSubstitution/relaxed)))

(define-member-substitution-dsl)


(defmacro ^:private define-element-matchers-dsl []
  (emit-dsl
    ElementMatchers
    "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/matcher/ElementMatchers.html"
    :static true))

(define-element-matchers-dsl)


(defmacro ^:private define-unloaded-dynamic-type-dsl []
  (emit-dsl
    DynamicType$Unloaded
    "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/dynamic/DynamicType.Unloaded.html"))

(define-unloaded-dynamic-type-dsl)


(defmacro ^:private define-loaded-dynamic-type-dsl []
  (emit-dsl
    DynamicType$Loaded
    "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/dynamic/DynamicType.Loaded.html"))

(define-loaded-dynamic-type-dsl)


(defmacro ^:private define-type-pool-dsl []
  (emit-dsl
    TypePool
    "See: https://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/pool/TypePool.html"))

(define-type-pool-dsl)


(defmacro ^:private define-type-pool-resolution-dsl []
  (emit-dsl
    TypePool$Resolution
    "See: https://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/pool/TypePool.Resolution.html"))

(define-type-pool-resolution-dsl)


(defmacro ^:private define-class-loading-strategy-default-dsl []
  (emit-dsl
    ClassLoadingStrategy$Default
    "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/dynamic/loading/ClassLoadingStrategy.Default.html"))

(define-class-loading-strategy-default-dsl)


(defn naming-strategy [kind-or-fn & [arg1 arg2]]
  (case kind-or-fn
    :prefixing-random (new NamingStrategy$PrefixingRandom arg1)
    :suffixing-random (new NamingStrategy$SuffixingRandom arg1)
    :auxiliary (case arg1
                 :suffixing-random
                 (new AuxiliaryType$NamingStrategy$SuffixingRandom arg2)
                 (proxy [AuxiliaryType$NamingStrategy] []
                   (name [type-description] (arg1 type-description))))
    (proxy [NamingStrategy] []
      (rebase   [type-description] (kind-or-fn type-description))
      (redefine [type-description] (kind-or-fn type-description))
      (subclass [superclass]       (kind-or-fn superclass)))))

(defn class-loading-strategy [kind]
  (case kind
    :child-first            ClassLoadingStrategy$Default/CHILD_FIRST
    :child-first-persistent ClassLoadingStrategy$Default/CHILD_FIRST_PERSISTENT
    :injection              ClassLoadingStrategy$Default/INJECTION
    :wrapper                ClassLoadingStrategy$Default/WRAPPER
    :wrapper-persistent     ClassLoadingStrategy$Default/WRAPPER_PERSISTENT
    :hotswap                (do (ByteBuddyAgent/install) ;; idempotent, see doc
                                (ClassReloadingStrategy/fromInstalledAgent))))
(defn type-pool [kind & [x]]
  (case kind
    :default         (TypePool$Default/of x)
    :boot-loader     (TypePool$Default/ofBootLoader)
    :platform-loader (TypePool$Default/ofPlatformLoader)
    :system-loader   (TypePool$Default/ofSystemLoader)))

(defn class-file-locator [kind & [x y :as args]]
  (case kind
    :compound     (ClassFileLocator$Compound.
                    (map #(cond (instance? ClassFileLocator %) %
                                (sequential? %) (apply class-file-locator %))
                         args))
    :simple       (if y
                    (ClassFileLocator$Simple/of x y)
                    (ClassFileLocator$Simple/of x))
    :class-loader (ClassFileLocator$ForClassLoader/of
                    (or x (-> (Thread/currentThread) .getContextClassLoader)))
    :of-resources (ClassFileLocator$Simple/ofResources x)))

(defn type-description [kind & [x y]]
  (case kind
    :javassist   (let [ct-class   x
                       class-name (.getName ct-class)
                       locator    (or y (class-file-locator
                                          :simple
                                          class-name (.toBytecode ct-class)))]
                   (-> (type-pool :default locator)
                       (describe class-name)
                       resolve))
    :loaded-type (TypeDescription$ForLoadedType. x)))
