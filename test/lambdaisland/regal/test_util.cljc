(ns lambdaisland.regal.test-util
  (:require [lambdaisland.regal :as regal])
  #?(:cljs (:require-macros [lambdaisland.regal.test-util :refer [inline-resource]])
     :clj (:require [clojure.java.io :as io])))

#?(:clj
   (defmacro inline-resource [resource-path]
     (read-string (slurp (io/resource resource-path)))))

(defn read-test-cases []
  #? (:clj (read-string (slurp (io/resource "lambdaisland/regal/test_cases.edn")))
      :cljs (inline-resource "lambdaisland/regal/test_cases.edn")))

(defn flavor [value]
  (or (some #{:java8 :java9 :java :ecma} (keys (meta value))) :common))

(defn flav-match? [value flav]
  (or (= :any flav)
      (isa? regal/flavor-hierarchy flav (flavor value))))

(defn filter-cases [cases flav]
  (for [[pattern form & tests :as case] cases
        :let [[canonical tests] (if (string? (first tests))
                                  [(first tests) (rest tests)]
                                  [pattern tests])]
        :when (flav-match? case flav)]
    {:pattern   pattern
     :form      form
     :canonical canonical
     :flavor    (flavor case)
     :tests     (for [test tests
                      :when (flav-match? test flav)]
                  test)}))

(defn test-cases
  ([]
   (test-cases regal/*flavor*))
  ([flav]
   (let [cases (read-test-cases)]
     (loop [[id & cases] cases
            result []]
       (if id
         (recur (drop-while vector? cases)
                (conj result
                      {:id id
                       :cases (filter-cases (take-while vector? cases) flav)}))
         result)))))

(test-cases)
