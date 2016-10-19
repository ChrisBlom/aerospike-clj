(ns aerospike-test
  (:require
   [aerospike :as as]
   [midje.sweet :refer :all]))

(def user-1-bins {:user/name "red"
                  :user/level 1000000
                  :user/items ["magic-wand","staff"]
                  :user/props {:strength 18
                               :intelligence 3
                               :wisdom 3}})

(def user-2-bins {:user/name "blue"
                          :user/level 1234
                          :user/items ["shield"]
                          :user/props {:strength 5
                                       :intelligence 12}})

;; Expects an aerospike server running on port 3000
(fact "about the aerospike client"
  (with-open [c (as/client (as/client-policy) "localhost" 3000)]

    (as/nodes c)
    => (just [(partial instance? com.aerospike.client.cluster.Node)])

    (as/node-names c)
    => (just [string?])

    (fact "aerospike keys can be constructed and inspected"

      (def user-1-key (as/key "test" "users" 1))
      (def user-2-key (as/key "test" "users" 2))

      (as/key->vector user-1-key)
      => ["test" "users" 1]


      (as/key-namespace user-1-key)
      => "test"

      (as/key-set-name user-1-key)
      => "users"

      (as/key-user-key user-1-key)
      => 1)


    (fact "as/bins converts bin keys to keywords"

      (as/delete c nil user-1-key)
      (as/put c user-1-key user-1-bins)

      (as/delete c nil user-2-key)
      (as/put c user-2-key user-2-bins)

      (as/bins (as/get c nil user-1-key))
      => user-1-bins

      (as/bins (as/get c nil user-2-key))
      => user-2-bins)

    (fact "policies for key-value operations can be nil"
      (as/get c nil user-1-key)

      (as/exists c nil user-1-key)

      (as/delete c nil user-2-key)

      (as/add c nil user-1-key {:user/level 10})

      ;; String operations
      (as/append c nil user-1-key {:user/name " player"})

      (as/prepend c nil user-1-key {:user/name "The "}))

    (fact "operate takes mutiple operations"
      (let [k (as/key "test" "multi-ops" 1)]
        (as/delete c nil k)
        (as/put c nil k {:counter 0 :string "(ツ)"})
        (-> (as/operate c nil k
                        [:add :counter 1]
                        [:append :string "_/¯"]
                        [:prepend :string "¯\\_"]
                        [:put :added "ok"]
                        :get)
            (as/bins)))
      => {:counter 1
          :added "ok"
          :string "¯\\_(ツ)_/¯"})

    (fact "you can specify which bins to fetch"
      (let [k (as/key "test" "specific-bins" 1)]
        (as/delete c nil k)
        (as/put c nil k {:a 1 :b 2 :c 3 :d 4})
        (as/bins (as/get c nil k :a :b)))
      => {:a 1 :b 2})

    (fact "the generation count can be set, retrieved and used in policies"
      (def k-generation (as/key "test" "expiration" 1))

      (as/delete c nil k-generation)
      (as/put c nil k-generation {:a 1})

      (as/generation (as/get c nil k-generation))
      => 1

      (as/add c nil k-generation {:a 1})
      (as/generation (as/get c nil k-generation))
      => 2

      (as/add c
              (as/write-policy {:generation 1337
                                :generation-policy :generation-policy/expect-gen-equal})
              k-generation {:a 1})
      => (throws com.aerospike.client.AerospikeException "Error Code 3: Generation error")

      (as/add c
              (as/write-policy {:generation 2
                                :generation-policy :generation-policy/expect-gen-equal})
              k-generation {:a 1})
      => nil)

    (fact "batch read operations are supported"
      (let [ks (for [i (range 0 10)]
                 (as/key "test" "batch" i))]

        (doseq [k ks]
          (as/put c (as/write-policy {:expiration -1}) k {:a 1}))

        (as/batch-exists c (as/batch-policy) ks)
        => (repeat 10 true)

        (map as/bins (as/batch-get c (as/batch-policy) ks))
        => (repeat 10 {:a 1})

        (map as/ttl (as/batch-header c nil ks))
        => (repeat 10 -1)))

    (fact "expirations for keys can be set and retrieved"
      (def k-expiration (as/key "test" "expiration" 1))

      (as/delete c nil k-expiration)
      (as/put c (as/write-policy {:expiration 3}) k-expiration {:a 1})

      (as/expiration (as/get c nil k-expiration))
      => pos?

      (as/ttl (as/header c nil k-expiration))
      => pos?

      (as/exists c nil k-expiration)
      => true

      (Thread/sleep (* 4 1000))

      (as/exists c nil k-expiration)
      => false)

    ))
