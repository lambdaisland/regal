(ns lambdaisland.regal.malli-test
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [malli.error :as me]
            [lambdaisland.regal.malli :as rm]))

(def deprecated-opts {:registry {:regal rm/regal-schema}})

(deftest deprecated-regal-malli-test
  (is (= [:regal [:+ "y"]] (m/form [:regal [:+ "y"]] deprecated-opts)))
  (is (= :regal (m/type [:regal [:+ "y"]] deprecated-opts)))
  (is (= true (m/validate [:regal [:+ "y"]] "yyy" deprecated-opts)))
  (is (= ["should match regex"] (me/humanize (m/explain [:regal [:+ "y"]] "xxx" deprecated-opts) deprecated-opts))))

(def opts {:registry {::rm/regal rm/rm-regal-schema}})

(deftest rm-regal-malli-test
  (is (= [::rm/regal [:+ "y"]] (m/form [::rm/regal [:+ "y"]] opts)))
  (is (= ::rm/regal (m/type [::rm/regal [:+ "y"]] opts)))
  (is (= true (m/validate [::rm/regal [:+ "y"]] "yyy" opts)))
  (is (= ["should match regex"] (me/humanize (m/explain [::rm/regal [:+ "y"]] "xxx" opts) opts))))
