(defproject org.clojars.tristefigure/shuriken "0.13.1"
  :description "TristeFigure's Clojure toolbox"
  :url "https://github.com/TristeFigure/shuriken"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [potemkin "0.4.3"]
                 ;; Upgrading tools.reader means updating Step 3 of
                 ;; src/shuriken/monkey_patches/syntax_quote.clj (the
                 ;; :exclude statement)
                 [org.clojure/tools.reader "1.0.5"]
                 [com.palletops/ns-reload "0.1.0"]])
