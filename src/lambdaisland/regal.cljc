(ns lambdaisland.regal
  (:require [lambdaisland.regal.pattern :as pattern]
            [lambdaisland.regal.generator :as generator]))

(defn regex [r]
  (pattern/regex r))

(defn gen [r]
  (generator/gen r))

(defn sample [r]
  (generator/sample r))
