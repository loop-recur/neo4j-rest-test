(ns neo4test.neo
	(:use midje.sweet
        [clojure.walk :only [keywordize-keys]]
        clojure.tools.logging)
  (:require [neo4test.neo-util :as n]
  					[clj-json.core :as json]))


(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})

(defn start-up
  "Reset or create the db with a query"
  [body]
  (n/start (:query body))
)

(defn neo-post
  "Take cypher query and replacement map and send response"
  [body]
  (let [q (body :query)
  			p (body :params)]
  	(let [res (n/cypher q p)
  				columns (distinct (flatten (map (partial map first) res)))
  				rows (map (partial map last) res)
  				]
  		(json-response {:columns columns :data rows})
  	)
  	
  )  
)
