(ns lambdaisland.regal.malli.generator-test
  (:require [clojure.test :refer [deftest  is ]]
            [lambdaisland.regal.malli :as rm]
            [lambdaisland.regal.malli.generator :as rmg]
            [malli.core :as m]
            [malli.error :as me]
            [malli.generator :as mg]))

(rmg/register-regal-generator {:type :regal})

(def deprecated-opts {:registry {:regal rm/regal-schema}})

(deftest deprecated-regal-generator-test
  (is (= ["y" "yy" "yyy" "yyy" "yy" "yyy" "yyyy" "yyy" "yyyy" "yyyyyy"]
         (mg/sample [:regal [:+ "y"]] (assoc deprecated-opts :seed 0)))))

(rmg/register-regal-generator)

(def opts {:registry {::rm/regal rm/rm-regal-schema}})

(deftest regal-generator-test
  (is (= ["y" "yy" "yyy" "yyy" "yy" "yyy" "yyyy" "yyy" "yyyy" "yyyyyy"]
         (mg/sample [::rm/regal [:+ "y"]] (assoc opts :seed 0)))))
