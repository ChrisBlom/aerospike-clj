(ns aerospike.reflect
  (:require [clojure.string :as str]))

(let [camelcase-pattern (re-pattern (str/join "|"
                                              ["(?<=[A-Z])(?=[A-Z][a-z])"
                                               "(?<=[^A-Z])(?=[A-Z])"
                                               "(?<=[A-Za-z])(?=[^A-Za-z])"]))]
  (defn ^String camel-case->kebab-case
    [s]
    (-> s
        (str/replace camelcase-pattern "-")
        (str/lower-case))))

;;;; Enum

(defn enum? [class]
  (and class
       (class? class)
       (contains? (supers class) java.lang.Enum)))

(defn enum-case->kebab-case [x]
  (->> (str/split x #"_")
       (map str/lower-case)
       (str/join "-")))

(defn kebab-case->enum-case [x]
  (->> (str/split x #"-")
       (map str/upper-case)
       (str/join "_")))

(defn enum->keyword [enum-value]
  (keyword (camel-case->kebab-case (.getSimpleName (.getDeclaringClass enum-value)))
           (enum-case->kebab-case (.name enum-value))) )

(defn coerce-enum [enum-class value]
  (cond (instance? enum-class value)
        value

        (keyword? value)
        (coerce-enum enum-class (name value))

        (string? value)
        (try (Enum/valueOf enum-class (kebab-case->enum-case value))
             (catch java.lang.IllegalArgumentException e
               (throw (ex-info "Cannot coerce enum value"
                               {:value value
                                :enum-class enum-class
                                :enum-values (vec (.getEnumConstants enum-class))
                                :keyword-values (mapv (comp (partial keyword (camel-case->kebab-case (.getSimpleName enum-class))) enum-case->kebab-case str) (.getEnumConstants enum-class))}))))
        :else (throw (ex-info "Cannot coerce to enum" {:value value
                                                       :enum-class enum-class
                                                       :enum-values (vec (.getEnumConstants enum-class))}))))

(def primitive-types (set (map (comp symbol str) (keys primitives-classnames))))

(defn get-field [class instance field-name]
  (.get (.getField class (str field-name)) instance))

(defn public-field-info
  [class]
  (let [instance (.newInstance class)]
    (sort-by :clj-name
             (for [{:keys [type flags name] :as m} (:members (clojure.reflect/reflect class))
                   :when (and type
                              (contains? flags :public)
                              (not (contains? flags :static)))
                   :let [type (or (primitive-types type)
                                  (resolve type))]]
               (cond-> {:clj-name (symbol (camel-case->kebab-case name))
                        :java-name name
                        :type type
                        :default (get-field class instance name)}
                 (enum? type) (assoc :enum (vec (sort (.getEnumConstants type)))))))))

(defn public-field-constructor-docstring [class field-info-map]
  (let [instance (.newInstance class)
        longest (apply max (map (comp count str keyword :clj-name) field-info-map))
        pad (fn [s]
              (->> (repeat \space)
                   (concat (str s))
                   (take longest)
                   (apply str)))]
    (with-out-str
      (println "Creates a" (str (.getSimpleName class) ",") "optionally configured by a map with any of these keys:")
      (doseq [ {:keys [clj-name type default enum]} field-info-map
              :let [default (if enum (enum->keyword default) default)]]
        (if enum
          (println "  " (pad (keyword clj-name)) "a" (.getSimpleName type) "or one of" (set (map enum->keyword enum)))
          (println "  " (pad (keyword clj-name)) "of type" type ))
        (println "  " (pad "") "default:" default)))))


(defmacro defn-public-fields-constructor
  "Takes a Class, defines a function that creates an instance of Class,
   optionally setting the public fields with values from a map with idiomatic keywords"
  [class]
  (let [full-class (resolve class)
        public-fields (public-field-info full-class)
        clj-class-name (symbol (camel-case->kebab-case (.getSimpleName ^Class full-class)))
        policy 'policy
        kwargs (vec (map :clj-name public-fields))
        docstring (public-field-constructor-docstring full-class public-fields)
        setters (for [ {:keys [java-name clj-name type enum] :as f} public-fields]
                  `(when ~clj-name
                     (set! (~(symbol (str ".-" java-name)) ~policy)
                           ~(if enum
                              `(coerce-enum ~type ~clj-name)
                              clj-name))))]
    `(defn ~clj-class-name
       ~docstring
       ([] (new ~class))
       ([{:keys ~kwargs}]
        (let [~policy (new ~class)]
          ~@setters
          ~policy)))))
