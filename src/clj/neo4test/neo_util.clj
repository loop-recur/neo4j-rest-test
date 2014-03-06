(ns neo4test.neo-util
  (:use clojure.tools.logging
        clojure.contrib.generic.functor
        clojure.walk)
  (:require [clojure.set :as set])
  (:import org.neo4j.cypher.javacompat.ExecutionEngine
           [org.neo4j.test TestGraphDatabaseFactory]
           org.neo4j.graphdb.factory.GraphDatabaseFactory
           org.neo4j.tooling.GlobalGraphOperations
           (org.neo4j.graphdb PropertyContainer
                              Node
                              Relationship
                              RelationshipType
                              Direction
                              Label)))

(defonce ^:dynamic *g* nil)
(defonce ^:dynamic *cypher* nil)
(defonce ^:dynamic *props* nil)
(defonce ^:dynamic *ggo* nil)

(declare create-database execute-query)

(def create-query (atom nil))

(defn get-db
  [query]
  (let [db (create-database query)]
    (reset! create-query (partial execute-query db))
    db
  )
)

(defn- execute-query
  "Just used for create right now..."
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

(defn start
  ([query]
    (do
      (if *g* (.shutdown *g*))
      (alter-var-root #'*props*
        (fn [_] {}))
      (alter-var-root #'*g*
        (fn [_] (get-db query)))
      (alter-var-root #'*cypher*
        (fn [_] (ExecutionEngine. *g*)))
      (alter-var-root #'*ggo*
        (fn [_] (GlobalGraphOperations/at *g*)))
      (-> (Runtime/getRuntime)
        (.addShutdownHook (Thread. #(.shutdown *g*)))))))

(defmacro tx
  [& body]
  `(let [ tx# (.beginTx *g*) ]
     (try
       (let [ val# (do ~@body)]
         (.success tx#)
         val#)
       (catch Exception e#
         (println (.getMessage e#)))
       (finally (.close tx#)))))


;;;;; PropertyContainers

(defprotocol PropertyContainerCRUD
  (remove!
    [self]
    "Remove/Delete a Element from the graph"))

(defprotocol NodeCRUD
  (node!*
    [self] [self labels]
    "Creates a node with/without a list of labels")
  (node
    [self] [self key val]
    "get a node by either id or label/key/value")
  (labels [self])
  (rels [self] [self arg] [self type dir])
  (labels! [self labels])
  (rm-labels! [self labels]))

(defprotocol RelationshipCRUD
  (reltype [self])
  (node-at [self] [self position])
  (juga!
    [from label to]
    "Join two Elements with an Edge/Relationship"))

(defn- reltype*
  [key]
  (reify RelationshipType
    (^String name [self] (name key))))

(defn- label*
  [key]
  (if (and (not (or (keyword? key)
                    (string? key)))
           (isa? (type key) Label))
    key
    (reify Label
      (^String name [self] (name key)))))

(defn- dir*
  [dir]
  (condp = (name dir)
      "out" Direction/OUTGOING
      "in" Direction/INCOMING
      "both" Direction/BOTH
      (throw (IllegalArgumentException.))))

(extend-protocol NodeCRUD
  Iterable
  (juga! [from label to]
    (for [elm from]
      (juga! elm label to)))
  (node
    ([self]
      (for [item self]
        (node item)))
    ([self keys values]
      (filter #(not (nil? %))
        (set
          (for [label self
              key keys
              value values]
            (node label key value))))))
  (labels [self]
    (for [elm self]
      (labels elm)))
  (labels! [self labels]
    (for [elm self]
      (labels! elm labels)))
  (rm-labels [self labels]
    (for [elm self]
      (rm-labels! elm labels)))
  Node
  (labels [self]
    (if (-> self type (isa? Node))
      (tx
        (map str (-> self .getLabels seq)))))
  (labels! [self labels]
    (first
      (for [label labels]
        (tx (.addLabel self (label* label))
            self))))
  (rm-labels! [self labels]
    (first
      (for [label labels]
        (tx (.removeLabel self (label* label))
            self))))
  (rels
    ([self]
      (tx
        (-> self .getRelationships seq)))
    ([self arg]
      (tx 
        (if (-> arg coll?)
          (-> self (.getRelationships (into-array RelationshipType (map reltype* arg))))
          (condp = (name arg)
            "out" (-> self (.getRelationships (dir* arg)))
            "in" (-> self (.getRelationships (dir* arg)))
            "both" (-> self (.getRelationships (dir* arg)))))))
    ([self type dir]
      (tx
        (-> self (.getRelationships (reltype* type) (dir* dir))))))
  Number
  (node 
    ([self]
      (tx (-> *g* (.getNodeById self))))
    ([self key val]
      (throw (IllegalArgumentException.))))
  Object
  (node
    ([self]
      (throw (IllegalArgumentException.)))
    ([self key val]
      (tx
        (first (-> *g*
          (.findNodesByLabelAndProperty (label* self) (name key) val))))))
  nil
  (node!*
    ([x]
      (tx (-> *g* .createNode)))
    ([x labels]
      (tx
        (-> *g*
          (.createNode (into-array Label (map label* labels)))))))
  (labels [self])
  (labels! [self labels])
  (rm-labels! [self labels])
  (rels
    ([self])
    ([self arg])
    ([self type dir])))

(defmacro node!
  [& properties]
  `(if-let [vec# (vector ~@properties)]
     (if (coll? (first vec#))
       (let [labels# (first vec#)
             props# (next vec#)]
         (if props#
           (apply muta! (node!* nil labels#) props#)
           (node!* nil labels#)))
       (muta! (node!* nil) ~@properties))
    (node!* nil)))

(extend-protocol PropertyContainerCRUD
  Iterable
  (remove! [self]
    (for [elm self]
      (remove! elm)))
  PropertyContainer
  (remove! [self]
    (tx
      (-> self .delete))))

(extend-protocol RelationshipCRUD
  Iterable
  (reltype [self]
    (for [elm self]
      (reltype elm)))
  (node-at
    ([self]
      (for [rel self]
        (-> rel node-at)))
    ([self position]
      (for [rel self]
        (-> rel (node-at position)))))
  Node
  (juga! [from label to]
    (when (and from to)
      (tx
        (-> from
          (.createRelationshipTo
            to (reltype* label))))))
  Relationship
  (reltype [self]
    (tx
      (-> self .getType .name)))
  (node-at
    ([self]
      (tx (-> self .getNodes seq)))
    ([self position]
      (tx
        (if (-> position type (isa? Node))
          (-> self (.getOtherNode position))
          (condp = (name position)
            "start" (-> self .getStartNode) 
            "end" (-> self .getEndNode))))))
  nil
  (reltype [self])
  (juga! [self])
  (node-at 
    ([self])
    ([self position])))


;;;;; Properties

(defprotocol PropertyCRUD
  (lege
    [self] [self key]
    "read the properties of an Element as a Clojure map")
  (carpe!
    [self] [self key] [self key val]
    "'pluck' the property out of the Element (removing it) and return its value")
  (muta!*
    [self key val]
    "Change the value of an Element's property by key")
  (claves
    [self]
    "Return just the keys of an Element's properties")
  (valores
    [self]
    "Return just the values of an Element's properties"))

(extend-protocol PropertyCRUD
  nil
  (lege
    ([self] self)
    ([self key] self))
  (carpe!
    ([self] self)
    ([self key] self)
    ([self key val] self))
  (muta!* [self key val] self)
  (claves [self] self)
  (valores [self] self)
  Object
  (lege
    ([self] self)
    ([self key] self))
  (carpe!
    ([self] self)
    ([self key] self)
    ([self key val] self))
  (muta!* [self key val] self)
  (claves [self] self)
  (valores [self] self)
  Iterable
  (lege
    ([self]
      (for [elm self]
        (lege elm)))
    ([self key]
      (for [elm self]
        (lege elm key))))
  (carpe!
    ([self]
      (for [elm self]
        (carpe! elm)))
    ([self key]
      (for [elm self]
        (carpe! elm key)))
    ([self key val]
      (for [elm self]
        (carpe! elm key val))))
  (muta!*
    [self key val]
    (doall (for [elm self]
      (muta!* elm key val))))
  (claves [self]
    (for [elm self]
      (claves elm)))
  (valores [self]
    (for [elm self]
      (valores elm)))
  PropertyContainer
  (lege
    ([self]
      (let [props (into {} (for [k (tx (.getPropertyKeys self))]
                             [(keyword k) (lege self k)]))]
        (-> props
          (assoc :_id (-> self .getId)
                 :_type (-> self class .getSimpleName (clojure.string/replace "Proxy" "")))
          (#(if (-> self type (isa? Relationship))
            (assoc %1 :_reltype (tx (-> self .getType .name))
                      :_start (tx (-> self .getStartNode .getId))
                      :_end (tx (-> self .getEndNode .getId)))
            (assoc %1 :_labels (tx (into [] (-> self labels)))))))))
    ([self key]
      (tx (-> self (.getProperty (name key))))))
  (carpe!
    ([self]
      (for [k (tx (.getPropertyKeys self))]
        (carpe! self k)))
    ([self key]
      (let [prop (lege self key)]
        (tx
          (-> self (.removeProperty (name key))))
        prop))
    ([self key val]
      (let [prop (lege self key)]
        (if (= val prop)
          (carpe! self key)))))
  (muta!* [self key val]
    (tx
      (let [key (name key)]
        (.setProperty self key val)
        self)))
  (claves [self]
    (tx
      (-> self .getPropertyKeys set)))
  (valores [self]
    (tx
      (-> self .getPropertyValues))))

(defn muta!
  [obj key val & more]
  (if (and more (even? (count more)))
    (apply muta! obj more))
  (muta!* obj key val))


;;;;; GlobalGraphOperations

(defn all-labels
  []
  (tx (into [] (-> *ggo* .getAllLabels))))

(defn all-nodes
  []
  (tx (into [] (-> *ggo* .getAllNodes))))

(defn all-nodes-by-label
  [label]
  (tx (into [] (-> *ggo* (.getAllNodesWithLabel (label* label))))))

(defn all-rels
  []
  (tx (into [] (-> *ggo* .getAllRelationships))))

(defn all-rel-types
  []
  (tx (into [] (-> *ggo* .getAllRelationshipTypes))))

(defn all-rels-by-type
  [type]
  (let [rels (all-rels)]
    (filter #(not (nil? %))
      (set
        (for [rel rels]
          (if (= (reltype rel) (name type))
            rel))))))

(defn map-to-hash [m]
  (reduce (fn [acc, k] (assoc acc k (.get m k) )) {} (.keySet m))
)

(defn fake-self [node]
   (str "http://localhost:7474/db/data/node/" (.getId node))
)

(defn -node-data [node] 
  (tx
    (reduce #(merge %1 %2) {}
      (for [k (.getPropertyKeys node)]
        (hash-map (keyword k) (.getProperty node k)))))
  )

(defmulti node-data class) 
(defmethod node-data org.neo4j.graphdb.Node [node]
  {:self (fake-self node) :data (-node-data node)}
)
(defmethod node-data org.neo4j.kernel.impl.core.NodeProxy [node]
  {:self (fake-self node) :data (-node-data node)}
)
(defmethod node-data clojure.lang.PersistentVector [nodes]
  (reduce (fn [acc n] (cons n acc) ) [] nodes)
)
(defmethod node-data org.neo4j.kernel.impl.core.RelationshipProxy [nodes]
  (-node-data nodes)
)
(defmethod node-data java.lang.String [nodes]
  nodes
)
(defmethod node-data java.lang.Long [nodes]
  nodes
)
(defmethod node-data :default [nodes]
  (try (.keySet nodes)
    (fmap node-data (map-to-hash nodes))
    (catch Exception d
      (try (map node-data nodes)
        (catch Exception e
          (println (.getMessage e))
          (.values nodes)
        )
      )
    )
  )
)

;;;;; Cypher

(defn cypher
  [query & more]
  (tx
    (let [has-params (map? (first more))
          callback (if has-params (second more) (first more))
          result (into [] (-> *cypher*
                            (.execute query
                              (if has-params
                                (let [params (first more)
                                      result (stringify-keys params)]
                                    result
                                  )
                                {}))))]
      (map (fn [row]
        (map (fn [column]
          (let [colname (.getKey column)]
            [colname (tx (node-data (get row colname)))]
          )
         ) (.entrySet row)
        )
      ) result))))
