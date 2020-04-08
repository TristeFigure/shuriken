(defproject org.clojars.tristefigure/shuriken "0.14.46"
  :description "TristeFigure's Clojure toolbox"
  :url "https://github.com/TristeFigure/shuriken"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [;; NOTE: 1.10.0 breaks shuriken.spec
                 [org.clojure/clojure "1.9.0"]

                 [potemkin "0.4.3"]
                 [com.palletops/ns-reload "0.1.0"]
                 [dance "0.1.2"]
                 [threading "0.3.4"] ;; TODO: remove when dance is updated
                 [lexikon "0.2.1"]

                 ;; For monkey patches
                 [robert/hooke "1.3.0"]
                 [org.javassist/javassist "3.26.0-GA"]
                 [net.bytebuddy/byte-buddy "1.9.12"]
                 [net.bytebuddy/byte-buddy-agent "1.9.12"]
                 [camel-snake-kebab "0.4.0"]

                 ;; For solver
                 [org.clojure/data.priority-map "0.0.7"]

                 ;; For lein-tools-deps
                 ; [org.clojure/tools.deps.alpha "0.5.398"]

                 ;; To topologically sort sequences
                 [ubergraph "0.5.0"]

                 ;; For nonograms
                 [net.cgrand/seqexp "0.6.2"]

                 ;; TODO: update this in dance then remove it.
                 [org.flatland/ordered "1.5.7"]]
  ;; To fix a goddamn bug on reload
  :aot [loom.graph ubergraph.core]
  ;; For syntax-quote monkey-patch
  :java-source-paths ["src/java"]
  :profiles {:dev {;:aot [shuriken.monkey-patch-test]
                   :java-source-paths ["test/java"]
                   :dependencies [[codox-theme-rdash "0.1.2"]]}}
  :plugins [;; Fox monkey patches
            [lein-jdk-tools "0.1.1"] ; TODO: move to clojure.deps
            [lein-codox "0.10.3"]]
  :codox {:source-uri "https://github.com/TristeFigure/shuriken/" \
                      "blob/{version}/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :themes [:rdash]})
