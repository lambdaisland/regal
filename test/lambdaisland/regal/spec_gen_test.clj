(ns lambdaisland.regal.spec-gen-test
  (:require [lambdaisland.regal.spec-alpha]
            [lambdaisland.regal :as regal]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as spec-gen]))

(comment
  (defspec generated-forms-can-be-converted
    (prop/for-all [regal (s/gen ::regal/form)]
      ;; test that generators generate valid regexes
      (try
        (prn regal)
        (regal/regex regal)
        (catch Exception _
          false))))

  (test #'generated-forms-can-be-converted)

  ;; overflows easily
  (binding [s/*recursion-limit* 3] (gen/generate (s/gen ::regal/form))))
