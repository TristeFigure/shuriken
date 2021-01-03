(defproject org.clojars.tristefigure/shuriken "0.14.48"
  :description "TristeFigure's Clojure toolbox"
  :url "https://github.com/TristeFigure/shuriken"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [; [org.clojure/clojure "1.9.0"
                 ;; NOTE: 1.10.0 breaks shuriken.spec
                 [org.clojure/clojure       "1.10.0"]
                 [org.clojure/clojurescript "1.10.520" :scope "provided"]

                 [potemkin "0.4.3"]
                 [com.palletops/ns-reload "0.1.0"]

                 [dance "0.1.3"]
                 [lexikon "0.2.1"]

                 ;; For monkey patches
                 [robert/hooke "1.3.0"]
                 [org.javassist/javassist "3.26.0-GA"]
                 [net.bytebuddy/byte-buddy "1.10.14"]
                 [net.bytebuddy/byte-buddy-agent "1.10.14"]
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
  :plugins [[lein-codox "0.10.3"]
            [lein-cljsbuild "1.1.7"]
            [lein-doo   "0.1.11"]
            ;; Fox monkey patches
            [lein-jdk-tools "0.1.1"]] ;; TODO: move to clojure.deps
  ; :prep-tasks ["compile" ["cljsbuild" "once"]]
  :cljsbuild {:builds {}}

  :profiles
  {:common {:cljsbuild
            {:builds
             {:main {:source-paths ["src"]
                     :compiler     {:target     :nodejs
                                    :main       shuriken.doo-test
                                    :output-to     "target/js/main/shuriken.js"
                                    :output-dir    "target/js/main/"
                                    :optimizations :none}}}}}
   :dev [:common
         {:aot [loom.graph ubergraph.core] ; shuriken.monkey-patch-test
          :java-source-paths ["test/java"]
          :dependencies [[codox-theme-rdash "0.1.2"]
                         ;; To find cljs/cljc test namespaces to run with doo
                         [org.clojure/tools.namespace "0.3.1"]]}]
   :test [:dev
          {:cljsbuild
           {:builds
            {:main {:source-paths ["test"]
                    :compiler     {:output-to  "target/js/test/shuriken.js"
                                   :output-dir "target/js/test/"}}}}}]
   :release [:common
             {:cljsbuild
              {:builds
               {:main {:jar      true
                       :compiler {:optimizations :advanced}}}}}]}
  :doo {:build        "main"
        :alias        {:default [:node]}}
  :aliases {"test"    ["with-profile" "test" ["do" ["test"] ["doo" "once"]]]
            "deploy"  ["with-profile" "release" "deploy"]
            "release" ["with-profile" "release" "release"]}

  :codox {:source-uri "https://github.com/TristeFigure/shuriken/" \
                      "blob/{version}/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :themes [:rdash]})
