(ns lambdaisland.regal.parse
  (:require [instaparse.core :as instaparse]
            #?(:clj [clojure.java.io :as io])
            [lambdaisland.regal :as regal]
            [lambdaisland.regal.platform :as platform]
            [clojure.string :as str])
  #?(:cljs
     (:require-macros [lambdaisland.regal.parse :refer [inline-resource]])))

#?(:clj
   (defmacro inline-resource [path]
     (slurp (io/resource path))))

(def java-grammar
  #?(:clj (io/resource "lambdaisland/regal/java.bnf")
     :cljs (inline-resource "lambdaisland/regal/java.bnf")))

(def ecma-grammar
  #?(:clj (io/resource "lambdaisland/regal/ecma.bnf")
     :cljs (inline-resource "lambdaisland/regal/ecma.bnf")))

(def parser*
  (memoize
   (fn [flavor]
     (cond
       (isa? regal/flavor-hierarchy flavor :java) (instaparse/parser java-grammar)
       (isa? regal/flavor-hierarchy flavor :ecma) (instaparse/parser ecma-grammar)))))

(defn parser []
  (parser* regal/*flavor*))

(defn ^:private remove-QE
  "Preprocesses a regex string (the same way that openjdk does) by
  transforming all \\Q...\\E expressions. Returns a string which is
  an equivalent regex that contains no \\Q...\\E expressions."
  {:author "Gary Fredericks"}
  [^String s]
  (if (str/includes? s "\\Q")
    (letfn [(remove-QE-not-quoting [chars]
              (lazy-seq
               (when-let [[c1 & [c2 :as cs]] (seq chars)]
                 (if (= \\ c1)
                   (if (= \Q c2)
                     (remove-QE-quoting-init (rest cs))
                     (list* c1 c2 (remove-QE-not-quoting (rest cs))))
                   (cons c1 (remove-QE-not-quoting cs))))))
            ;; I don't understand why this clause in the java code,
            ;; but it is, and this is what it does, and it has some
            ;; weird effects, like in #"\c\Q0"
            (remove-QE-quoting-init [chars]
              (when-let [[c1 :as cs] (seq chars)]
                (if (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} c1)
                  (list* \\ \x \3 c1
                         (remove-QE-quoting (rest cs)))
                  (remove-QE-quoting cs))))
            (remove-QE-quoting [chars]
              (lazy-seq
               (when-let [[c1 & [c2 :as cs]] (seq chars)]
                 (if (and (= c1 \\) (= c2 \E))
                   (remove-QE-not-quoting (rest cs))
                   (if (or (re-matches #"[0-9a-zA-Z]" (str c1))
                           (<= 128 (int c1) ))
                     (cons c1 (remove-QE-quoting cs))
                     (list* \\ c1 (remove-QE-quoting cs)))))))]
      (apply str (remove-QE-not-quoting s)))
    s))

(defn collapse-strings-xf
  "Join any consecutive strings in a sequence, transducer"
  [rf]
  (let [strings (volatile! [])
        flush (fn [acc]
                (when (seq @strings)
                  (rf acc (apply str @strings))
                  (vreset! strings [])))
        process (fn [acc x]
                  (if (string? x)
                    (do
                      (vswap! strings conj x)
                      acc)
                    (do
                      (flush acc)
                      (rf acc x))))]
    (fn
      ([] (rf))
      ([acc]
       (flush acc)
       (rf acc))
      ([acc x]
       (process acc x)))))

(defn subvec-replace [s pat repl]
  (let [cnt (count pat)]
    (loop [s s
           acc []]
      (cond
        (< (count s) cnt)
        (into acc s)
        (= pat (take cnt s))
        (recur (drop cnt s) (into acc repl))
        :else
        (recur (next s) (conj acc (first s)))))))

(defmulti transform (fn [tree] [(cond
                                  (vector? tree) (first tree)
                                  (string? tree) :string
                                  :else :default)
                                regal/*flavor*])
  :hierarchy #'regal/flavor-hierarchy)

(defmethod transform [:string :common] [s] s)
(defmethod transform :default [t]
  (if (map? t)
    {::parse-error t}
    [::not-implemented t]))

(defmethod transform [:Regex :common] [[_ f]] (transform f))
(defmethod transform [:SingleExpr :common] [[_ f]] (transform f))
(defmethod transform [:BaseExpr :common] [[_ f]] (transform f))
(defmethod transform [:LiteralChar :common] [[_ f]] (transform f))
(defmethod transform [:PlainChar :common] [[_ f]] (transform f))
(defmethod transform [:EscapedChar :common] [[_ f]] (transform f))
(defmethod transform [:HexChar :common] [[_ f]] (transform f))
(defmethod transform [:BasicEscapedChar :common] [[_ f]] (transform f))
(defmethod transform [:CharExpr :common] [[_ x]] (transform x))
(defmethod transform [:Dot :common] [[_ x]] :any)
(defmethod transform [:Anchor :common] [[_ char1 char2 :as anchor]]
  (case char1
    "^"
    :start
    "$"
    :end
    "\\"
    (case char2
      "A"
      :start
      "z"
      :end
      [::not-implemented anchor])
    [::not-implemented anchor]))

(defmethod transform [:DanglingCurlyRepetitions :common] [[_ & reps]])

;; Regal semantics for :line-breaks correspond with Java 8 semantics for \R. On
;; Java 9 the semantics differ, so we map \R to a regal expression that matches
;; the Java 9 semantics.
(defmethod transform [:LinebreakMatcher :java8] [[_ & reps]]
  :line-break)

(defmethod transform [:LinebreakMatcher :java9] [[_ & reps]]
  [:alt
   [:cat [:char 13] [:char 10]]
   [:class
    [:char 10]
    [:char 11]
    [:char 12]
    [:char 13]
    [:char 133]
    [:char 8232]
    [:char 8233]]])

(defmethod transform [:LinebreakMatcher :ecma] [[_ & reps]]
  (throw (ex-info "Line break character \\R is not supported in ECMAScript regular expressions."
                  {::not-supported {:flavor :ecma :feature :line-break :pattern "\\R"}})))

(def line-break-equivalent
  "For :java9 and :ecma we simulate \\R when generating patterns, when parsing we
  want to round trip, so we detect this specific form."
  [:alt
   [:cat :return :newline]
   [:cat
    [:negative-lookahead [:cat :return :newline]]
    [:class [:newline :return] [:char 133] [:char 8232] [:char 8233]]]])

(def whitespace-chars
  [[:char 9]
   [:char 10]
   [:char 11]
   [:char 12]
   [:char 13]
   [:char 32]
   [:char 160]
   [:char 5760]
   [:char 8192]
   [:char 8193]
   [:char 8194]
   [:char 8195]
   [:char 8196]
   [:char 8197]
   [:char 8198]
   [:char 8199]
   [:char 8200]
   [:char 8201]
   [:char 8202]
   [:char 8232]
   [:char 8233]
   [:char 8239]
   [:char 8287]
   [:char 12288]])

(def whitespace-equivalent (into [:class] whitespace-chars))
(def neg-whitespace-equivalent (into [:not] whitespace-chars))

(def non-whitespace-ranges
  [[[:char 0] [:char 8]]
   [[:char 14] [:char 31]]
   [[:char 33] [:char 159]]
   [[:char 161] [:char 5759]]
   [[:char 5761] [:char 8191]]
   [[:char 8203] [:char 8231]]
   [[:char 8234] [:char 8238]]
   [[:char 8240] [:char 8286]]
   [[:char 8288] [:char 12287]]
   [[:char 12289] [:char 65535]]])

(def vertical-whitespace-equivalent
  [:class :newline [:char 11] :form-feed :return [:char 133] [:char 8232] [:char 8233]])

(defmethod transform [:Alternation :common] [[_ & alts]]
  (let [alts (map transform alts)]
    (if (= (count alts) 1)
      (first alts)
      (let [result (into [:alt] (remove nil?) alts)]
        (if (and (some regal/current-flavor? [:java9 :ecma])
                 (= result line-break-equivalent))
          :line-break
          result)))))

(defmethod transform [:Concatenation :common] [[_ & cats]]
  (let [cats (sequence (comp (map transform) (remove nil?) collapse-strings-xf) cats)]
    (case (count cats)
      0 nil
      1 (first cats)
      (into [:cat] cats))))

(defn conj-form [f1 f2]
  (if (regal/tagged-form? :cat f2)
    (into f1 (next f2))
    (conj f1 f2)))

(def lazified {:? :??
               :+ :+?
               :* :*?})

(defn lazify [f quantifier]
  (if (= quantifier "?")
    (get lazified f)
    f))

(defmethod transform [:SuffixedExpr :common] [[_ expr suffix :as x]]
  (if suffix
    (let [[_SuffixedExp [suffix-type curly-min _ curly-max] [_Quantifier quantifier]] suffix
          form (case suffix-type
                 :Optional
                 (conj-form [(lazify :? quantifier)] (transform expr))
                 :Positive
                 (conj-form [(lazify :+ quantifier)] (transform expr))
                 :NonNegative
                 (conj-form [(lazify :* quantifier)] (transform expr))
                 :CurlyRepetition
                 (if curly-max
                   [:repeat
                    (transform expr)
                    (platform/parse-int curly-min)
                    (platform/parse-int curly-max)]
                   [:repeat (transform expr) (platform/parse-int curly-min)])
                 [::not-implemented x])]
      form)
    (transform expr)))

(defmethod transform [:ControlChar :common] [[_ [ch]]]
  [:ctrl (str ch)])

(defn transform-normal-slached-characters [[_ [_ ch] :as x]]
  (case ch
    \n
    :newline
    \r
    :return
    \t
    :tab
    \f
    :form-feed
    #_:else
    ch))

(defmethod transform [:NormalSlashedCharacters :common] [x]
  (transform-normal-slached-characters x))

(defmethod transform [:NormalSlashedCharacters :java] [[_ [_ ch] :as x]]
  (case ch
    \a
    :alert
    \e
    :escape
    #_:else
    (transform-normal-slached-characters x)))

(defmethod transform [:SpecialCharClass :java] [[_ [ch] :as x]]
  (case ch
    \v
    :vertical-whitespace
    \d
    :digit
    \D
    :non-digit
    \w
    :word
    \W
    :non-word
    \s
    [:class " " :tab :newline :vertical-tab :form-feed :return]
    \S
    [:not " " :tab :newline :vertical-tab :form-feed :return]
    #_:else
    [::not-implemented x]))

(defmethod transform [:SpecialCharClass :ecma] [[_ [ch] :as x]]
  (case ch
    \v
    :vertical-tab
    \d
    :digit
    \D
    :non-digit
    \w
    :word
    \W
    :non-word
    \s
    :whitespace
    \S
    :non-whitespace
    [::not-implemented (pr-str ch) x]))

(defmethod transform [:ShortHexChar :java] [[_ x]]
  (if(= "0B" x)
    :vertical-tab
    [:char (platform/hex->int x)]))

(defmethod transform [:ShortHexChar :ecma] [[_ x]]
  (case x
    "07"
    :alert
    "1B"
    :escape
    [:char (platform/hex->int x)]))

(defmethod transform [:Null :ecma] [_]
  :null)

(defmethod transform [:MediumHexChar :common] [[_ x]] [:char (platform/hex->int x)])

;; Character classes, there's a lot to unpack
(defmethod transform [:BCC :common] [[_ x]] (transform x))
(defmethod transform [:BCCIntersection :common] [[_ x]] (transform x))

(defn tranform-bcc-union-left  [[_ & xs]]
  (if (= (ffirst xs) :BCCNegation)
    (into [:not] (comp (map transform) collapse-strings-xf) (next xs))
    (into [:class] (comp (map transform) collapse-strings-xf) xs)))

(defmethod transform [:BCCUnionLeft :java] [x]
  (let [result (tranform-bcc-union-left x)]
    ;; Some special case handling to make sure we round-trip expansions
    ;; of :whitespace
    (cond
      (= result whitespace-equivalent)
      :whitespace
      (= result neg-whitespace-equivalent)
      :non-whitespace
      :else
      (-> result
          (subvec-replace whitespace-chars [:whitespace])
          (subvec-replace non-whitespace-ranges [:non-whitespace])))))

(defmethod transform [:BCCUnionLeft :ecma] [x]
  (let [result (tranform-bcc-union-left x)]
    (if (= result vertical-whitespace-equivalent)
      :vertical-whitespace
      result)))

(defmethod transform [:BCCElemHardLeft :common] [[_ x]] (transform x))
(defmethod transform [:BCCElemNonLeft :common] [[_ x]] (transform x))
(defmethod transform [:BCCElemBase :common] [[_ x]] (transform x))
(defmethod transform [:BCCCharNonRange :common] [[_ x]] (transform x))
(defmethod transform [:BCCCharEndRange :common] [[_ x]] (transform x))
(defmethod transform [:BCCChar :common] [[_ x]] (transform x))
(defmethod transform [:BCCPlainAmpersand :common] [[_ x]] "&")
(defmethod transform [:BCCPlainChar :common] [[_ x]] (transform x))
(defmethod transform [:BCCRange :common] [[_ x y]]
  (let [from (transform x)
        to   (transform y)]
    [(if (char? from) (str from) from)
     (if (char? to) (str to) to)]))

(defmethod transform [:GroupFlags :common] [[_ g :as x]]
  (case (first g)
    :NonCapturingMatchFlags      [:non-capturing-group (transform g)]
    :PositiveLookAheadFlag       [:lookahead]
    :NegativeLookAheadFlag       [:negative-lookahead]
    :PositiveLookBehindFlag      [:lookbehind]
    :NegativeLookBehindFlag      [:negative-lookbehind]
    :IndependentNonCapturingFlag [:atomic]
    :NamedCapturingGroup         [::not-implemented x]))

(defmethod transform [:NonCapturingMatchFlags :common] [[_ x]] (transform x))
(defmethod transform [:MatchFlagsExpr :common] [[_ & xs]] xs)
(defmethod transform [:NonCapturingGroup :common] [[_ x]] (transform x))

(defmethod transform [:ParenthesizedExpr :common] [[_ x y]]
  (if y
    (let [[type flags] (transform x)
          form (transform y)]
      (case type
        :non-capturing-group
        form
        [type form]))
    (conj-form [:capture] (transform x))))

(defn parse-pattern [pattern]
  (->> pattern
       remove-QE
       (instaparse/parse (parser))
       transform))

(defn parse [regex]
  (parse-pattern (regal/regex-pattern regex)))

(comment
  (for [p ["[x]" "[xy]" "[x-y]" "[^x-y]" "[x-y_]"]]
    [p (parse p)])

  [(parse "(x)")
   (parse "(?:x)")
   ;;(parse "(?<name>x)")
   ;;(parse "(?idmsuxU)x")

   (parse "(?<foo)")
   (parse "(?>foo)")
   (parse "(?-idmsuxU:x)")]

  (parse "[aeiou]")

  (require 'lambdaisland.regal.test-util)
  (for [{:keys [cases]} (lambdaisland.regal.test-util/test-cases)
        {:keys [pattern form]} cases]
    (if (map? pattern)
      (for [[flavor pattern] pattern]
        [flavor (regal/with-flavor flavor [form (parse pattern)])])
      [form (parse pattern)])
    )

  (instaparse/parse (parser) "a")

  (run! regal/regex )
  (prn (s/gen :lambdaisland.regal/form))

  (regal/regex [:cat "abc"])
  (instaparse/parse (parser) "(?!x)")

  (parse "\\u000D\\u000A|[\\u000A\\u000B\\u000C\\u000D\\u0085\\u2028\\u2029]")

  (regal/with-flavor :ecma
    (instaparse/parse (parser) "(?!x)"))

  (time (parser))

  )
