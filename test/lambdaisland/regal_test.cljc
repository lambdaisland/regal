(ns lambdaisland.regal-test
  (:require [lambdaisland.regal :as regal]
            [lambdaisland.regal.spec-alpha]
            [lambdaisland.regal.test-util :as test-util]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest testing is are]]))

(stest/instrument `regal/regex)

(deftest regex-test
  (is (= "abc"
         (regal/pattern (regal/regex [:cat "a" "b" "c"]))))

  (is (= "a|b|c"
         (regal/pattern (regal/regex [:alt "a" "b" "c"]))))

  (is (= "a*"
         (regal/pattern (regal/regex [:* "a"]))))

  (is (= "(?:ab)*"
         (regal/pattern (regal/regex [:* "ab"]))))

  (is (= "(?:ab)*"
         (regal/pattern (regal/regex [:* "a" "b"]))))

  (is (= "(?:a|b)*"
         (regal/pattern (regal/regex [:* [:alt "a" "b"]]))))

  (is (= "a+"
         (regal/pattern (regal/regex [:+ "a"]))))

  (is (= "a?"
         (regal/pattern (regal/regex [:? "a"]))))

  (is (= "[a-z0-9_-]"
         (regal/pattern (regal/regex [:class [\a \z] [\0 \9] \_ \-]))))

  (is (= "[^a-z0-9_-]"
         (regal/pattern (regal/regex [:not [\a \z] [\0 \9] \_ \-]))))

  (is (= "a{3,5}"
         (regal/pattern (regal/regex [:repeat \a 3 5]))))

  (regal/with-flavor :ecma
    (is (= "^a$"
           (regal/pattern (regal/regex [:cat :start \a :end])))))

  (regal/with-flavor :java
    (is (= "\\Aa\\z"
           (regal/pattern (regal/regex [:cat :start \a :end])))))

  (is (= "a(?:b|c)"
         (regal/pattern (regal/regex [:cat "a" [:alt "b" "c"]]))))

  (is (= "(abc)"
         (regal/pattern (regal/regex [:capture "abc"]))))

  (is (= "a(b|c)"
         (regal/pattern (regal/regex [:cat "a" [:capture [:alt "b" "c"]]])))))

(deftest escape-test
  (are [in out] (= out (regal/escape in))
    "$" "\\$"
    "(" "\\("
    ")" "\\)"
    "*" "\\*"
    "+" "\\+"
    "." "\\."
    "?" "\\?"
    "[" "\\["
    "]" "\\]"
    "\\" "\\\\"
    "^" "\\^"
    "{" "\\{"
    "|" "\\|"
    "}" "\\}"))

(deftest data-based-tests
  (doseq [{:keys [id cases]} (test-util/test-cases :any)
          {:keys [canonical form flavor tests]} cases]

    (testing (str (name id) " " form " " flavor)
      (regal/with-flavor flavor
        (is (= canonical (regal/pattern (regal/regex form)))))

      (when (isa? (regal/runtime-flavor) flavor)
        (doseq [[input match :as test] tests
                :when (test-util/flav-match? test (regal/runtime-flavor))]
          (testing (str (pr-str form) " matches " (pr-str input))
            (is (= match (re-find (regal/regex form) input)))))))))
