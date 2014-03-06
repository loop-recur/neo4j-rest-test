(ns neo4test.routes
  (:use compojure.core
        ring.middleware.json-params
        neo4test.neo
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [clj-json.core :as json]
            [compojure.response :as response]))


(defn simple-logging-middleware [app]
  (fn [req]
    (println req)
    (app req)))

(defroutes main-routes
  (POST "/db/data/batch" [& x] (start-up x))
  (POST "/db/data/cypher" [& x] (neo-post x))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (-> (handler/site main-routes)
      wrap-base-url
      wrap-json-params))