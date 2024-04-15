(ns lambdaisland.regal.malli-test
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [malli.error :as me]
            [lambdaisland.regal.malli :as rm]))

(def opts {:registry {::rm/regal (rm/regal-schema)}})

(deftest regal-malli-test
  (is (= [::rm/regal [:+ "y"]] (m/form [::rm/regal [:+ "y"]] opts)))
  (is (= ::rm/regal (m/type [::rm/regal [:+ "y"]] opts)))
  (is (= true (m/validate [::rm/regal [:+ "y"]] "yyy" opts)))
  (is (= ["should match regex"] (me/humanize (m/explain [::rm/regal [:+ "y"]] "xxx" opts) opts))))
