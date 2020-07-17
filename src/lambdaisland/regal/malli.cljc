(ns lambdaisland.regal.malli
  (:require [lambdaisland.regal :as regal]
            [lambdaisland.regal.generator :as generator]
            [malli.core :as m]
            [malli.generator :as mg]))

(def regal-schema
  ^{:type ::into-schema}
  (reify m/IntoSchema
    (-into-schema [_ properties [regal :as children] options]
      (when-not (= 1 (count children))
        (m/fail! ::child-error {:type :regal, :properties properties, :children children, :min 1, :max 1}))
      (let [form (m/create-form :regal properties children)
            regex (regal/regex regal)]
        ^{:type ::schema}
        (reify
          m/Schema
          (-type [_] :regal)
          (-validator [_]
            (fn [x] (try (boolean (re-find regex x)) (catch #?(:clj Exception, :cljs js/Error) _ false))))
          (-explainer [this path]
            (fn [x in acc]
              (try
                (if-not (re-find regex x)
                  (conj acc (m/error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (m/error path in this x (:type (ex-data e))))))))
          (-transformer [this transformer method options]
            (m/-value-transformer transformer this method options))

          (-walk [this walker in options]
            (when (m/-accept walker this in options)
              (m/-outer walker this (vec children) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)

          mg/Generator
          (-generator [this options]
            (generator/gen regal)))))))

(comment
  (require '[malli.core :as m]
           '[malli.error :as me]
           '[malli.generator :as mg]
           '[lambdaisland.regal.malli :as regal-malli])

  (def malli-opts {:registry {:regal regal-malli/regal-schema}})

  (def form [:+ "y"])

  (def schema (m/schema [:regal form] malli-opts))

  (m/form schema)
  ;; => [:regal [:+ "y"]]

  (m/type schema)
  ;; => :regal

  (m/validate schema "yyy")
  ;; => true

  (me/humanize (m/explain schema "xxx"))
  ;; => ["unknown error"]

  (me/humanize (m/explain schema "xxx") {:errors {:regal {:error/message {:en "Pattern does not match"}}}})
  ;; => ["Pattern does not match"]

  (mg/sample schema)
  ;; => ("y" "yy" "yy" "yyyy" "yyyy" "y" "yyy" "yyyyy" "yyyyyyyyy" "yyyyyyyy")
  )
