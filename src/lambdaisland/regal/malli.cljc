(ns lambdaisland.regal.malli
  (:require [lambdaisland.regal :as regal]
            [lambdaisland.regal.generator :as generator]
            [malli.core :as m]))

(def regal-schema
  ^{:type ::m/into-schema}
  (reify m/IntoSchema
    (-into-schema [_ properties [regal :as children] options]
      (m/-check-children! :regal properties children {:min 1, :max 1})
      (let [form (m/-create-form :regal properties children)
            regex (regal/regex regal)]
        ^{:type ::m/schema}
        (reify
          m/Schema
          (-type [_] :regal)
          (-type-properties [_]
            {:error/message {:en "Pattern does not match"}
             :gen/gen (generator/gen regal)
             :json-schema {:type "string", :pattern (str regex)}})
          (-validator [_]
            (fn [x] (try (boolean (re-find regex x)) (catch #?(:clj Exception, :cljs js/Error) _ false))))
          (-explainer [this path]
            (fn [x in acc]
              (try
                (if-not (re-find regex x)
                  (conj acc (m/-error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (m/-error path in this x (:type (ex-data e))))))))
          (-transformer [this transformer method options]
            (m/-value-transformer transformer this method options))
          (-walk [this walker in options]
            (when (m/-accept walker this in options)
              (m/-outer walker this (vec children) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form))))))

(comment
  (require '[malli.core :as m]
           '[malli.error :as me]
           '[malli.generator :as mg]
           '[malli.json-schema :as mj]
           '[lambdaisland.regal.malli :as regal-malli])

  ;; use directly
  (def schema [regal-malli/regal-schema [:+ "y"]])

  ;; or via a registry
  (def registry {:regal regal-malli/regal-schema})
  (def schema (m/schema [:regal [:+ "y"]] {:registry registry}))

  (m/form schema)
  ;; => [:regal [:+ "y"]]

  (m/type schema)
  ;; => :regal

  (m/validate schema "yyy")
  ;; => true

  (m/explain schema "xxx")
  ;; => {:schema [:regal [:+ "y"]]
  ;;     :value "xxx"
  ;;     :errors (#Error{:path [], :in [], :schema [:regal [:+ "y"]], :value "xxx"})}

  (me/humanize (m/explain schema "xxx"))
  ;; => ["Pattern does not match"]

  (mj/transform schema)
  ;; => {:type "string", :pattern "y+"}

  (mg/sample schema)
  ;; => ("y" "yy" "yy" "yyyy" "yyyy" "y" "yyy" "yyyyy" "yyyyyyyyy" "yyyyyyyy")
  )
