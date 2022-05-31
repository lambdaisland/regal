(ns ^:deprecated lambdaisland.regal.malli
  "Custom Malli schema that supports regal directly. No longer compatible with
  latest Malli, see the README for an alternative way to combine the two."
  (:require [lambdaisland.regal :as regal]
            [lambdaisland.regal.generator :as generator]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.error :as me]
            [malli.impl.util :as miu]))

(def regal-schema
  ^{:type ::into-schema}
  (reify m/IntoSchema
    (-type [_] :regal)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties [regal :as children] options]
      (m/-check-children! :regal properties children {:min 1, :max 1})
      (let [form (m/-create-form :regal properties children)
            regex (regal/regex regal)]
        ^{:type ::schema}
        (reify
          m/Schema
          (-validator [_]
            (fn [x] (try (boolean (re-find regex x)) (catch #?(:clj Exception, :cljs js/Error) _ false))))
          (-explainer [this path]
            (fn [x in acc]
              (try
                (if-not (re-find regex x)
                  (conj acc (miu/-error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (miu/-error path in this x (:type (ex-data e))))))))
          (-transformer [this transformer method options]
            (m/-value-transformer transformer this method options))
          (-parser [_])
          (-unparser [_])
          (-walk [this walker in options]
            (when (m/-accept walker this in options)
              (m/-outer walker this (vec children) in options)))
          (-properties [_] (merge {:error/message {:en "Pattern does not match"}} properties))
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          (-parent [_] parent)
          mg/Generator
          (-generator [this options]
            (generator/gen regal))

          #_me/SchemaError
          #_(-error [this]
              {:error/message {:en "Pattern does not match"}}))))))

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
  ;; => ["Pattern does not match"]

  (mg/sample schema)
  ;; => ("y" "yy" "yy" "yyyy" "yyyy" "y" "yyy" "yyyyy" "yyyyyyyyy" "yyyyyyyy")
  )
