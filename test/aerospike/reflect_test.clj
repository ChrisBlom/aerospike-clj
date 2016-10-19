(ns aerospike.reflect-test
  (:use aerospike.reflect
        midje.sweet)
  (:import [com.aerospike.client.policy.WritePolicy]))

(fact "coerce enum is flexible"
  (= (coerce-enum com.aerospike.client.policy.CommitLevel com.aerospike.client.policy.CommitLevel/COMMIT_ALL)
     (coerce-enum com.aerospike.client.policy.CommitLevel "COMMIT_ALL")
     (coerce-enum com.aerospike.client.policy.CommitLevel :commit-level/commit-all)
     (coerce-enum com.aerospike.client.policy.CommitLevel :commit-all))
  => true)

(fact "public-field-info knows which values are valid for enum fields"
  (->> com.aerospike.client.policy.WritePolicy
       public-field-info
       (filter (comp #{'commitLevel} :java-name))
       first
       :enum)
  => (vec (.getEnumConstants com.aerospike.client.policy.CommitLevel)))

(fact "generated public-fields-constructor have docstrings"

  (defn-public-fields-constructor com.aerospike.client.policy.WritePolicy)
  => #'write-policy

  (:doc (meta #'write-policy))
  => #".*WritePolicy.*"

  (type (write-policy))
  => com.aerospike.client.policy.WritePolicy

  (type (write-policy {:expiration 20}))
  => com.aerospike.client.policy.WritePolicy

  (.-expiration (write-policy))
  => 0

  (.-expiration (write-policy {:expiration 123}))
  => 123

  (.-commitLevel (write-policy))
  => com.aerospike.client.policy.CommitLevel/COMMIT_ALL

  (.-commitLevel (write-policy {:commit-level :commit-level/commit-master}))
  => com.aerospike.client.policy.CommitLevel/COMMIT_MASTER

  )
