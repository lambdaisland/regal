(ns lambdaisland.regal-test
  (:require [lambdaisland.regal :as regal]
            [lambdaisland.regal.spec-alpha]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest testing is are]]))

(stest/instrument `regal/regex)

(defn reg-str
  "Regex to string, remove the slashes that JavaScript likes to add."
  [r]
  #?(:clj (str r)
     :cljs (let [s (str r)
                 len (count s)]
             (.substring s 1 (dec len)))))

(deftest regex-test
  (is (= #?(:clj "\\Qa\\E\\Qb\\E\\Qc\\E"
            :cljs "abc")
         (reg-str (regal/regex [:cat "a" "b" "c"]))))

  (is (= #?(:clj "\\Qa\\E|\\Qb\\E|\\Qc\\E"
            :cljs "a|b|c")
         (reg-str (regal/regex [:alt "a" "b" "c"]))))

  (is (= #?(:clj "\\Qa\\E*"
            :cljs "a*")
         (reg-str (regal/regex [:* "a"]))))

  (is (= #?(:clj "(?:\\Qab\\E)*"
            :cljs "(?:ab)*")
         (reg-str (regal/regex [:* "ab"]))))

  (is (= #?(:clj "(?:\\Qa\\E\\Qb\\E)*"
            :cljs "(?:ab)*")
         (reg-str (regal/regex [:* "a" "b"]))))

  (is (= #?(:clj "(?:\\Qa\\E|\\Qb\\E)*"
            :cljs "(?:a|b)*")
         (reg-str (regal/regex [:* [:alt "a" "b"]]))))

  (is (= #?(:clj "\\Qa\\E+"
            :cljs "a+")
         (reg-str (regal/regex [:+ "a"]))))

  (is (= #?(:clj "\\Qa\\E?"
            :cljs "a?")
         (reg-str (regal/regex [:? "a"]))))

  (is (= "[a-z]"
         (reg-str (regal/regex [:range \a \z]))))

  (is (= "[a-z0-9_-]"
         (reg-str (regal/regex [:class [\a \z] [\0 \9] \_ \-]))))

  (is (= "[^a-z0-9_-]"
         (reg-str (regal/regex [:not [\a \z] [\0 \9] \_ \-]))))

  (is (= #?(:clj "\\Qa\\E{3,5}"
            :cljs  "a{3,5}")
         (reg-str (regal/regex [:repeat \a 3 5]))))

  (is (= #?(:clj "\\A\\Qa\\E\\z"
            :cljs "^a$")
         (reg-str (regal/regex [:cat :start \a :end]))))

  (is (= #?(:clj "\\Qa\\E(?:\\Qb\\E|\\Qc\\E)"
            :cljs "a(?:b|c)")
         (reg-str (regal/regex [:cat "a" [:alt "b" "c"]]))))

  (is (= #?(:clj "(\\Qabc\\E)"
            :cljs "(abc)")
         (reg-str (regal/regex [:capture "abc"]))))

  (is (= #?(:clj "\\Qa\\E(\\Qb\\E|\\Qc\\E)"
            :cljs "a(b|c)")
         (reg-str (regal/regex [:cat "a" [:capture [:alt "b" "c"]]]))))
  )
