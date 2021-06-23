(ns lambdaisland.regal.spec-gen-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as spec-gen]
            [clojure.test :refer [deftest is are testing run-tests]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.properties :as prop']
            [com.gfredericks.test.chuck.regexes.charsets :as charsets]
            [lambdaisland.regal :as regal]
            [lambdaisland.regal.generator :as regal-gen]
            [lambdaisland.regal.test-util :refer [re2-find re2-compile]]
            [lambdaisland.regal.parse :as parse]
            [lambdaisland.regal.spec-alpha :as regal-spec]))

(def form-gen (s/gen ::regal/form))
(def canonical-form-gen (gen/fmap regal/normalize (s/gen ::regal/form)))

(defspec generated-forms-can-be-converted 100
  (prop/for-all [regal form-gen]
                (try
                  (regal/regex regal)
                  (catch Exception _
                    false))))

(defn- round-trip? [form]
  (= form (parse/parse (regal/regex form))))

(defspec round-trip-property 100
  (prop/for-all* [canonical-form-gen] round-trip?))

(deftest round-trip-test
  (is (round-trip? [:cat "   " [:class "&& "]]))
  (is (round-trip? [:class " " [" " "["]]))
  (is (round-trip? [:ctrl "A"]))
  (is (round-trip? [:class "   - "]))
  (is (round-trip? [:alt "  " [:capture " " :escape]]))
  (is (round-trip? :whitespace))
  (is (round-trip? [:? [:? "x"]]))
  (is (round-trip? [:cat "  " [:class " " :non-whitespace]]))
  (is (round-trip? [:cat "-" [:repeat [:repeat "x" 0] 0]])))

(def token->charset-map
  (let [whitespace-charset (apply charsets/union
                                  (map (comp charsets/singleton str char) regal/whitespace-char-codes))]
    {:any charsets/all-unicode-but-line-terminators
     :digit (charsets/predefined-regex-classes \d)
     :non-digit (charsets/predefined-regex-classes \D)
     :word (charsets/predefined-regex-classes \w)
     :non-word (charsets/predefined-regex-classes \W)
     :whitespace whitespace-charset
     :non-whitespace (charsets/difference
                      (charsets/intersection charsets/all-unicode
                                             (charsets/range "\u0000" "\uFFFF"))
                      whitespace-charset)
     :newline (charsets/singleton "\n")
     :return (charsets/singleton "\r")
     :tab (charsets/singleton "\t")
     :form-feed (charsets/singleton "\f")
     :alert (charsets/singleton "\u0007")
     :escape (charsets/singleton "\u001B")
     :vertical-whitespace (charsets/predefined-regex-classes \v)
     :vertical-tab (charsets/singleton "\u000B")
     :null (charsets/singleton "\u0000")}))

(defn token->charset [token]
  (or (get token->charset-map token)
      (throw (ex-info "Unknown token type" {:token token}))))

(defn class->charset [cls]
  (reduce charsets/union*
          charsets/empty
          (for [c cls]
            (try
              (cond
                (vector? c)
                (let [[start end] (map str c)]
                  (assert (>= 0 (compare start end)))
                  (charsets/range start end))

                (simple-keyword? c)
                (token->charset c)

                (string? c)
                (reduce charsets/union*
                        (map (comp charsets/singleton str) c))

                (char? c)
                (charsets/singleton (str c)))
              (catch Exception e
                (throw (ex-info "Failed to translate class element into charset" 
                                {:cls cls
                                 :element c}
                                e)))))))

(defn class->gen [[op & elts :as expr]]
  (let [cls (class->charset elts)
        cls (case op
              :not (charsets/difference charsets/all-unicode cls)
              :class cls

              (throw (ex-info "Unknown character class op" {:op op})))]
    (if (nat-int? (charsets/size cls))
      (gen/fmap #(charsets/nth cls %) (gen/choose 0 (dec (charsets/size cls))))
      (throw (ex-info "Can't generate empty class" {:expr expr})))))

(defmethod regal-gen/-generator :not [r opts]
  (class->gen r))

(defmethod regal-gen/-generator :class [r opts]
  (class->gen r))

(defn gen-carefully [fgen else-gen]
  (try
    (let [gen (fgen)]
      (gen/->Generator
       (fn [rnd size]
         (try
           (gen/call-gen gen rnd size)
           (catch Exception _
             (gen/call-gen else-gen rnd size))))))
    (catch Exception _
      else-gen)))

(defn can-generate? [regal]
  (try
    (gen/sample(regal-gen/gen regal))
    true
    (catch Exception _
      false)))

(defspec re2-matches-like-java 10
  (with-redefs [regal-spec/token-gen #(s/gen (disj regal-spec/known-tokens :line-break :start :end))]
    (prop'/for-all [regal form-gen
                    :when (can-generate? regal)
                    s (gen-carefully #(regal-gen/gen regal)
                                     gen/string)
                    :let [java-result
                          (try (re-find (regal/regex regal) s)
                               (catch Exception _
                                 :fail))]
                    :when (not= :fail java-result)]
                   (is (= java-result
                          (re2-find (regal/with-flavor :re2
                                      (re2-compile (regal/pattern regal)))
                                    s))))))
