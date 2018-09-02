(defproject org.clojars.tristefigure/shuriken "0.14.8"
  :description "TristeFigure's Clojure toolbox"
  :url "https://github.com/TristeFigure/shuriken"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [potemkin "0.4.3"]
                 [com.palletops/ns-reload "0.1.0"]
                 [dance "0.1.1"]
                 [lexikon "0.1.0"]

                 ;; For monkey patches
                 [robert/hooke "1.3.0"]
                 [org.javassist/javassist "3.20.0-GA"]

                 ;; For solver
                 [org.clojure/data.priority-map "0.0.7"]

                 ;; For lein-tools-deps
                 ; [org.clojure/tools.deps.alpha "0.5.398"]

                 ;; To topologically sort sequences
                 [ubergraph "0.5.0"]]
  ;; To fix a goddamn bug on reload
  :aot [loom.graph ubergraph.core]
  ;; For syntax-quote monkey-patch
  :java-source-paths ["src/java"]
  :profiles {:dev {:aot [shuriken.monkey-patch-test]
                   :java-source-paths ["test/java"]
                   :dependencies [[codox-theme-rdash "0.1.2"]]}}
  :plugins [;; Fox monkey patches
            [lein-jdk-tools "0.1.1"] ; TODO: move to clojure.deps
            [lein-codox "0.10.3"]]
  :codox {:source-uri "https://github.com/TristeFigure/shuriken/" \
                      "blob/{version}/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :themes [:rdash]})
