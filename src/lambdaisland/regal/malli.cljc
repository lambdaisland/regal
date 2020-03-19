(ns lambdaisland.regal.malli
  (:require [lambdaisland.regal :as regal]
            [malli.core :as m]))

(defn- -regal-schema []
  ^{:type ::into-schema}
  (reify m/IntoSchema
    (-into-schema [_ properties children options]
      (when-not (= 1 (count children))
        (m/fail! ::child-error {:name :vector, :properties properties, :children children, :min 1, :max 1}))
      (let [form (m/create-form :regal properties children)
            ]
        ^{:type ::schema}
        (reify m/Schema
          (-name [_] :regal)
          (-validator [_]
            )
          (-explainer [this path]
            #_(fn explain [x in acc]
                (if-not (or (nil? x) (validator' x)) (conj acc (error path in this x)) acc)))
          (-transformer [this transformer method options]
            #_(let [this-transformer (-value-transformer transformer this method options)
                    child-transformer (-transformer schema' transformer method options)
                    build (fn [phase]
                            (let [->this (phase this-transformer)
                                  ->child (phase child-transformer)]
                              (if (and ->this ->child)
                                (comp ->child ->this)
                                (or ->this ->child))))]
                {:enter (build :enter)
                 :leave (build :leave)}))
          (-accept [this visitor options] #_(visitor this [(m/-accept schema' visitor options)] options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] form)
          #_LensSchema
          #_(-get [_ key default] (if (= 0 key) schema' default))
          #_(-set [_ key value] (if (= 0 key) (into-schema :maybe properties [value]) schema')))))))

(def into-schema
  (#'m/-leaf-schema
   :regal
   (fn [properties children _]
     (when-not (= 1 (count children))
       (m/fail! ::child-error {:name :vector, :properties properties, :children children, :min 1, :max 1}))
     (let [regal-expr (first children)
           regex (regal/regex regal-expr)]
       [(fn [s]
          (and (string? s)
               (re-find regex s)))
        children]))))

(def registry
  {:regal into-schema})

(comment
  (require '[malli.core :as m]
           '[lambdaisland.regal.malli :as regal-malli])

  (m/validate [:regal [:+ "x"]] "xxx" {:registry regal-malli/registry})
  ;;=> "xxx"
  )
