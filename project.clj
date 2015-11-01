(defproject sanningens-silverflod "0.1.0-SNAPSHOT"
  :description "Constraint Handling Rules on DataScript"
  :url "http://github.com/hraberg/sanningens-silverflod"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.145"]
                 [org.clojure/core.async "0.2.371"]
                 [datascript "0.13.2"]]
  :pedantic? :abort
  :plugins [[lein-cljsbuild "1.1.0"]])
