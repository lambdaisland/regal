(ns lambdaisland.regal.malli.generator
  (:require [lambdaisland.regal.generator :as generator]
            [lambdaisland.regal.malli :as rm]
            [malli.core :as m]
            [malli.generator :as mg]))

(defn regal-generator
  "Returns a generator for a ::rm/regal schema."
  [schema options]
  (let [regal (second (m/form schema options))]
    (generator/gen regal)))

(defn register-regal-generator
  "Registers generator for ::rm/regal schema.
  
  A custom m/type instead of ::rm/regal can be provided with the :type argument."
  ([] (register-regal-generator nil))
  ([{:keys [type]
     :or {type rm/default-regal-type}}]
   (defmethod mg/-schema-generator type
     [schema options]
     (regal-generator schema options))))
