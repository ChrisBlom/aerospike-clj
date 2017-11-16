(defproject clj-aerospike "0.1.0-SNAPSHOT"
  :description "Clojure wrapper for the Aerospike Java client"
  :url "http://github.com/ChrisBlom/clj-aerospike"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :repositories [["jitpack" "https://jitpack.io"]]

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [com.aerospike/aerospike-client "3.2.5"]]

  :profiles {:dev {:dependencies [[midje "1.8.3"]]
                   :plugins [[lein-midje "3.2.1"]]}})
