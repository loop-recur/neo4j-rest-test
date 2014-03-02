(defproject neo4test "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.match "0.2.0"]
                 [compojure "1.1.6"]
                 [ring "1.2.1"]
                 [org.clojure/tools.logging "0.2.6"]
                 [ring-json-params "0.1.0"]
                 [clj-json "0.2.0"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]
                                  #_[org.neo4j/neo4j-kernel "2.0.0"]
                                  [org.neo4j/neo4j-kernel "2.0.0"
                                   :classifier "tests"]
                                  [org.neo4j/neo4j-cypher "2.0.0"]
                                  [org.clojure/core.async "0.1.267.0-0d7780-alpha"]]}}
  :plugins [[lein-cljsbuild "1.0.1"]
            [lein-ring "0.8.7"]]
  :ring {:handler neo4test.routes/app}
  :main neo4test.core)
