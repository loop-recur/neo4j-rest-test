(ns neo4test.neo
	(:use midje.sweet
        [clojure.walk :only [keywordize-keys]]
        clojure.tools.logging)
  (:import [org.neo4j.test TestGraphDatabaseFactory]
           [org.neo4j.cypher.javacompat ExecutionEngine])
  (:require [neo4test.neo-util :as n]
  					[clj-json.core :as json]))

(declare create-database execute-query)

(def create-query (atom nil))

(defn start-up
	[body]
	(let [blah (warn body)
        db (create-database (:query body))]
		(reset! create-query (partial execute-query db))
		(n/start db)
	)
)

(defn- execute-query
  "Executes a cypher query and returns the results.
   This doesnt use named params, so should never be used in prod"
  [db cypher]
  (let [engine (ExecutionEngine. db)
        result (.execute engine cypher)]
    (sequence result)))

(defn- create-database
  "Creates and returns database using the cypher passed in"
  [cypher]
  (let [db (.newImpermanentDatabase (TestGraphDatabaseFactory. ))
        engine (ExecutionEngine. db)]
    (.execute engine cypher)
    db))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})

(defn neo-post
  "Add actual cypher test"
  [body]
  (let [q (body :query)
  			p (body :params)]
  	(let [res (n/cypher q p)
  				columns (map first res)
  				rows (map last res)
  				]
  		(json-response {:columns columns :data [rows]})
  	)
  	
  )  
)
