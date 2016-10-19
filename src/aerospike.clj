(ns aerospike
  (:refer-clojure :as clj :exclude [key get])
  (:require [aerospike.reflect :refer :all])
  (:import
   [com.aerospike.client AerospikeClient Bin Host Key Operation Record Value]
   [com.aerospike.client.policy BatchPolicy InfoPolicy Policy QueryPolicy ScanPolicy WritePolicy]))

;;;; helper functions, to ease type hinting

(defn- key-array ^"[Lcom.aerospike.client.Key;" [keys] (into-array Key keys))

(defn- bin-array ^"[Lcom.aerospike.client.Bin;" [keys] (into-array Bin keys))

(defn- string-array ^"[Ljava.lang.String;" [strings] (into-array String strings))

;;;; Bin

(defprotocol ToBinKey
  (bin-key ^java.lang.String [k]))

(extend-protocol ToBinKey
  java.lang.String
  (bin-key [k]
    k)
  clojure.lang.Keyword
  (bin-key [k] (if-let [ns (namespace k)]
                 (str ns "/" (name k))
                 (name k))))


(defn bin ^com.aerospike.client.Bin [k v]
  (Bin. (bin-key k) v))

(defn- map->bin-array ^"[Lcom.aerospike.client.Bin;" [map]
  (let [bins (for [ [k v] map]
               (Bin. (bin-key k) v))]
    (bin-array bins)))

;;;; Record

(defn bins
  "takes a Record, returns the (selected) bins as a map"
  ([^Record record]
   (when record
     (into {} (for [[k v] (.-bins record)]
                [(keyword k) v]))))
  ([^Record record & selected-bins]
   (when record
     (into {} (for [[k v] (select-keys (.-bins record) (map bin-key selected-bins))]
                [(keyword k) v])))))

(defn expiration
  [^Record record]
  (some-> record (.-expiration)))

(defn generation [^Record record]
  (some-> record (.-generation)))

(defn ttl
  "record ttl in seconds"
  [^Record record]
  (some-> record
          (.getTimeToLive)))

;;;; Client

(defn client
  (^com.aerospike.client.AerospikeClient [policy ^String host port]
   (if policy
     (AerospikeClient. policy host (int port))
     (AerospikeClient. host (int port))))
  (^com.aerospike.client.AerospikeClient [policy host-port-map]
    (AerospikeClient. ^com.aerospike.client.policy.ClientPolicy policy
                      (->> host-port-map
                           (map (fn [[host port]] (Host. host (int port))))
                           ^"[Lcom.aerospike.client.Host;" (into-array Host)))))

(defn connected? [^AerospikeClient client]
  (.isConnected client))

(defn nodes [^AerospikeClient client]
  (vec (.getNodes client)))

(defn node-names [^AerospikeClient client]
  (vec (.getNodeNames client)))

;;;; Policies

(defn-public-fields-constructor com.aerospike.client.policy.AdminPolicy)
(defn-public-fields-constructor com.aerospike.client.policy.BatchPolicy)
(defn-public-fields-constructor com.aerospike.client.policy.ClientPolicy)
(defn-public-fields-constructor com.aerospike.client.policy.InfoPolicy)
(defn-public-fields-constructor com.aerospike.client.policy.Policy)
(defn-public-fields-constructor com.aerospike.client.policy.QueryPolicy)
(defn-public-fields-constructor com.aerospike.client.policy.ScanPolicy)
(defn-public-fields-constructor com.aerospike.client.policy.WritePolicy)

;;;; Key-Value Operations

(defn put
  ([^AerospikeClient client key bin-map]
   (.put client nil key (map->bin-array bin-map)))
  ([^AerospikeClient client ^WritePolicy write-policy key bin-map]
   (.put client write-policy key (map->bin-array bin-map))))

(defn add
  ([^AerospikeClient client key bin-map]
   (.add client nil key (map->bin-array bin-map)))
  ([^AerospikeClient client ^WritePolicy write-policy key bin-map]
   (.add client write-policy key (map->bin-array bin-map))))

(defn get ^com.aerospike.client.Record
  ([^AerospikeClient client ^Policy policy ^Key key]
   (.get client policy key))
  ([^AerospikeClient client ^Policy policy ^Key key & bin-keys]
   (.get client policy key (string-array (map bin-key bin-keys)))))

(defn batch-get
  ([^AerospikeClient client ^BatchPolicy batch-policy keys]
   (vec (.get client batch-policy (key-array keys))))
  ([^AerospikeClient client ^BatchPolicy batch-policy keys & bin-keys]
   (vec (.get client batch-policy (key-array keys) (string-array (map bin-key bin-keys))))))

(defn touch [^AerospikeClient client ^WritePolicy write-policy ^Key key]
  (.touch client write-policy key))

(defn header ^com.aerospike.client.Record
  [^AerospikeClient client ^Policy policy ^Key key]
  (.getHeader client policy key))

(defn batch-header
  [^AerospikeClient client ^BatchPolicy batch-policy keys]
  (.getHeader client batch-policy (key-array keys)))

(defn ^com.aerospike.client.Record exists
  [^AerospikeClient client ^Policy policy ^Key key]
  (.exists client policy key))

(defn ^com.aerospike.client.Record batch-exists
  [^AerospikeClient client ^BatchPolicy batch-policy keys]
  (vec (.exists client batch-policy (key-array keys))))

(defn append
  ([^AerospikeClient client key bin-map]
   (.append client nil key (map->bin-array bin-map)))
  ([^AerospikeClient client ^WritePolicy write-policy key bin-map]
   (.append client write-policy key (map->bin-array bin-map))))

(defn prepend
  ([^AerospikeClient client key bin-map]
   (.prepend client nil key (map->bin-array bin-map)))
  ([^AerospikeClient client ^WritePolicy write-policy key bin-map]
   (.prepend client write-policy key (map->bin-array bin-map))))

(def optable
  (->> [kw (case  (:parameter-types m)
                    [com.aerospike.client.Bin]
                    {:args 2
                     :fn (eval `(fn ~(symbol name) [k# v#] (~(symbol "Operation" (str name)) (bin k# v#))))}
                    [java.lang.String]
                    {:args 1
                     :fn (eval `(fn [str#] (~(symbol "Operation" (str name)) str#)))}
                    {:args 0
                     :fn (eval `(fn [] (~(symbol "Operation" (str name)))))})]
       (for [{:keys [name] :as m} (:members (clojure.reflect/reflect Operation))
             :when (= (:return-type m) `Operation)
             :let [kw (keyword (camel-case->kebab-case name))]])
       (into {})))

(defn convert-op
  [op]
  (cond
    (vector? op) (if-let [op-map (optable (first op))]
                   (do (assert (= (:args op-map) (dec (count op))) (str "Operation " (first op) " takes " (:args op-map) " arguments"))
                       (apply (:fn op-map) (rest op)))
                   (throw (ex-info "Unsupported keyword" {:operation op} )))
    (keyword? op) (convert-op [op])
    :else (throw (java.lang.IllegalArgumentException. "Must be :keyword or [:keyword & args]"))))

(defn operate [^AerospikeClient client write-policy key & operations]
  (.operate client write-policy key (into-array Operation (map convert-op operations))))

(defn delete [^AerospikeClient client write-policy key]
  (.delete client write-policy key))



(defprotocol ToKey
  (-key [key ^String namespace ^String set]))

(extend-protocol ToKey
  ;; byte arrays
  (Class/forName "[B") (-key [key namespace set]  (Key. ^String namespace ^String set ^bytes key))
  String (-key [key namespace set]  (Key. ^String namespace ^String set ^String key ))
  java.lang.Long (-key [key namespace set]  (Key. ^String namespace ^String set (long key) ))
  java.lang.Integer (-key [key namespace set]  (Key. ^String namespace ^String set (int key)))
  Value (-key [value namespace set]  (Key. ^String namespace ^String set ^Value value)))

(defn key
  [namespace set key]
  (-key key namespace set))

(defn key->vector [^Key key]
  [(.namespace key)
   (.setName key)
   (.getObject (.userKey key))])

(defn key-user-key
  "takes an Aerospike key, returns its key"
  [^Key key]
  (.getObject (.userKey key)))

(defn key-namespace
  "takes an Aerospike key, returns its namespace"
  [^Key key]
  (.namespace key))

(defn key-set-name
  "takes an Aerospike key, returns the names of its set"
  [^Key key]
  (.setName key))
