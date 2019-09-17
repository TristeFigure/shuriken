(ns shuriken.byte-buddy.dsl
  (:refer-clojure :exclude [require name merge not load])
  (:require [camel-snake-kebab.core :refer [->kebab-case-symbol]]
            [shuriken.reflection :refer [instance-methods static-methods]])
  (:import [java.lang.reflect Method]
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
            DynamicType$Loaded]
           [net.bytebuddy.dynamic.loading
            ClassLoadingStrategy
            ClassLoadingStrategy$Default
            ClassReloadingStrategy]
           [net.bytebuddy.asm
            MemberSubstitution]
           [net.bytebuddy.matcher
            ElementMatchers]))

(def ^:private alphabet
  (map (comp symbol str char) (range (int \a) (inc (int \z)))))

(defn emit-dsl [class gen-doc & {:keys [static]}]
  (let [class-name (-> class .getName symbol)]
    `(do ~@(for [ms (->> (if static
                           (static-methods class)
                           (instance-methods class))
                         (group-by (memfn ^Method getName))
                         vals
                         (map #(->> %
                                    (group-by (memfn ^Method getParameterCount))
                                    vals
                                    (map first))))
                 :let [method-name (-> ms first .getName)
                       fn-name     (-> method-name ->kebab-case-symbol)]]
             `(defn ~fn-name
                ~(gen-doc ms)
                ~@(for [m ms
                        :let [args (take (.getParameterCount m) alphabet)]]
                    `([~@(when-not static [(with-meta 'buddy {:tag class-name})])
                       ~@args]
                      ~(if static
                         `(~(->> method-name (str class-name "/") symbol)
                             ~@args)
                         `(~(->> method-name (str ".") symbol)
                             ~'buddy ~@args)))))))))


(defmacro ^:private define-byte-buddy-dsl []
  (emit-dsl
    ByteBuddy
    (constantly
      "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/ByteBuddy.html")))

(defn buddy
  ([]                   (new ByteBuddy))
  ([class-file-version] (new ByteBuddy class-file-version)))

(define-byte-buddy-dsl)


(defmacro ^:private define-dynamic-type-builder-dsl []
  (emit-dsl
    DynamicType$Builder
    (constantly
      "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/dynamic/DynamicType.Builder.html")))

(define-dynamic-type-builder-dsl)


(defmacro ^:private define-member-substitution-dsl []
  (emit-dsl
    MemberSubstitution
    (constantly
      "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/asm/MemberSubstitution.html")))

(defn member-substitution [mode]
  (case mode
    :strict  (MemberSubstitution/strict)
    :relaxed (MemberSubstitution/relaxed)))

(define-member-substitution-dsl)


(defmacro ^:private define-element-matchers-dsl []
  (emit-dsl
    ElementMatchers
    (constantly
      "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/matcher/ElementMatchers.html")
    :static true))

(define-element-matchers-dsl)


(defmacro ^:private define-unloaded-dynamic-type-dsl []
  (emit-dsl
    DynamicType$Unloaded
    (constantly
      "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/dynamic/DynamicType.Unloaded.html")))

(define-unloaded-dynamic-type-dsl)


(defmacro ^:private define-loaded-dynamic-type-dsl []
  (emit-dsl
    DynamicType$Loaded
    (constantly
      "See: http://bytebuddy.net/javadoc/1.9.12/net/bytebuddy/dynamic/DynamicType.Loaded.html")))

(define-loaded-dynamic-type-dsl)


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
