(ns round-trip
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [lambdaisland.regal :as regal]
            [lambdaisland.regal.parse :as parse]
            [lambdaisland.regal.spec-alpha :as rs]
            [lambdaisland.regal.platform :as platform]))

(gen/sample

 (s/gen :lambdaisland.regal/form))

(defn join-strings [v]
  (reduce (fn [v e]
            (if (and (string? (last v)) (string? e))
              (update v (dec (count v)) str e)
              (conj v e)))
          [] v))

(defn normalize [form]
  (cond
    (vector? form)
    (cond
      (and (= 2 (count form)) (= :cat (first form)))
      (recur (second form))

      (keyword? (first form))
      (let [joined (join-strings (mapv normalize form))]
        (if (not= joined form)
          (recur joined)
          form))

      :else
      (mapv normalize form))

    (char? form)
    (str form)

    :else
    form))

(check/quick-check
 100
 (prop/for-all [form (gen/fmap normalize (s/gen :lambdaisland.regal/form))]
   (= form (parse/parse (regal/regex form))))
 )

(normalize [:cat "  "])

(regal/pattern [:not " " :whitespace])

(regal/compile "[\\R]")

(parse/parse
 (regal/regex [:* " " :escape]))
;; => [:* [:cat " " :escape]]
