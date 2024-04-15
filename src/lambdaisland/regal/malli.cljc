(ns lambdaisland.regal.malli
  "Custom Malli schema that supports regal directly."
  (:refer-clojure :exclude [type])
  (:require [lambdaisland.regal :as regal]
            [malli.core :as m]
            [malli.util :as mu]))

(defn ->regal-schema
  "Add to registry with {:registry {::rm/regal (rm/->regal-schema)}}.

  e.g., [:rm/regal [:+ \"y\"]]
  
  A custom m/type can be provided with the :type argument:
    {:registry {:regal (rm/regal-schema {:type :regal)}}}
  e.g., [:regal [:+ \"y\"]]"
  ([] (->regal-schema nil))
  ([{:keys [type]
     :or {type ::regal}}]
   (mu/-util-schema
     {:type type :min 1 :max 1
      :childs 0 ;; no schema children
      :fn (fn [_ [regal :as c] _]
            (let [regex (regal/regex regal)]
              [[regex] ;; children
               c ;; forms
               (m/schema [:re regex])]))})))

(def
  ^{:doc "Consider using rm-regal-schema as it is namespaced to avoid registry clashes.
         
         Add to registry with {:registry {:regal rm/regal-schema}}.

         e.g., [regal [:+ \"y\"]]

         To register a generator, use (lambdaisland.regal.malli.generator/register-regal-generator {:type :regal})."
    :deprecated "0.0.144"
    :superseded-by "rm-regal-schema"}
  regal-schema (->regal-schema {:type :regal}))

(def rm-regal-schema
  "Add to registry with {:registry {::rm/regal rm-regal-schema}}.

  e.g., [::rm/regal [:+ \"y\"]]
  
  To register a generator, use (lambdaisland.regal.malli.generator/register-regal-generator)."
  (->regal-schema))

(comment
  (require '[malli.core :as m]
           '[malli.error :as me]
           '[malli.generator :as mg]
           '[lambdaisland.regal.malli :as regal-malli])

  (def malli-opts {:registry {::regal (regal-malli/regal-schema)}})

  (def schema (m/schema [::regal [:+ "y"]] malli-opts))

  (m/form schema)
  ;; => [::regal [:+ "y"]]

  (m/type schema)
  ;; => ::regal

  (m/validate schema "yyy")
  ;; => true

  (me/humanize (m/explain schema "xxx"))
  ;; => ["Pattern does not match"]

  (mg/sample schema)
  ;; => ("y" "yy" "yy" "yyyy" "yyyy" "y" "yyy" "yyyyy" "yyyyyyyyy" "yyyyyyyy")
  )
