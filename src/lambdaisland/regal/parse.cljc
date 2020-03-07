(ns lambdaisland.regal.parse
  (:require [instaparse.core :as instaparse]
            #?(:clj [clojure.java.io :as io])
            [lambdaisland.regal :as regal]
            [lambdaisland.regal.platform :as platform])
  #?(:cljs
     (:require-macros [lambdaisland.regal.parse :refer [inline-resource]])))

#?(:clj
   (defmacro inline-resource [path]
     (slurp (io/resource path))))

(def grammar
  #?(:clj (io/resource "lambdaisland/regal/regex.bnf")
     :cljs (inline-resource "lambdaisland/regal/regex.bnf")))

(def parser (delay (instaparse/parser grammar)))

(defn ^:private remove-QE
  "Preprocesses a regex string (the same way that openjdk does) by
  transforming all \\Q...\\E expressions. Returns a string which is
  an equivalent regex that contains no \\Q...\\E expressions."
  {:author "Gary Fredericks"}
  [^String s]
  (if (.contains s "\\Q")
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

(defmulti transform (fn [tree] [(cond
                                  (vector? tree) (first tree)
                                  (string? tree) :string
                                  :else :default)
                                regal/*flavor*])
  :hierarchy #'regal/flavor-hierarchy)

(defmethod transform [:string :common] [s] s)
(defmethod transform :default [t] [::not-implemented t])
(defmethod transform [:Regex :common] [[_ f]] (transform f))
(defmethod transform [:SingleExpr :common] [[_ f]] (transform f))
(defmethod transform [:BaseExpr :common] [[_ f]] (transform f))
(defmethod transform [:LiteralChar :common] [[_ f]] (transform f))
(defmethod transform [:PlainChar :common] [[_ f]] (transform f))
(defmethod transform [:EscapedChar :common] [[_ f]] (transform f))
(defmethod transform [:HexChar :common] [[_ f]] (transform f))
(defmethod transform [:BasicEscapedChar :common] [[_ f]] (transform f))
(defmethod transform [:CharExpr :common] [[_ x]] (transform x))

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

(defmethod transform [:Alternation :common] [[_ & alts]]
  (let [alts (map transform alts)]
    (if (= (count alts) 1)
      (first alts)
      (into [:alt] (remove nil?) alts))))

(defmethod transform [:Concatenation :common] [[_ & cats]]
  (let [cats (sequence (comp (map transform) (remove nil?) collapse-strings-xf) cats)]
    (case (count cats)
      0 nil
      1 (first cats)
      (into [:cat] cats))))

(defmethod transform [:SuffixedExpr :common] [[_ expr suffix]]
  (if suffix
    (throw (ex-info "" {::not-implemented [:SuffixedExpr expr suffix]}))
    (transform expr)))

(defmethod transform [:ControlChar :common] [[_ [ch]]]
  [:ctrl ch])

(defmethod transform [:NormalSlashedCharacters :common] [[_ [_ ch]]]
  (case ch
    \n
    :newline
    \r
    :return
    \t
    :tab
    \f
    :form-feed))

(defmethod transform [:NormalSlashedCharacters :java] [[_ [_ ch] :as exp]]
  (case ch
    \a
    :alert
    \e
    :escape
    (regal/with-flavor :common (transform exp))))

(defmethod transform [:ShortHexChar :common] [[_ x]] [:char (platform/hex->int x)])
(defmethod transform [:MediumHexChar :common] [[_ x]] [:char (platform/hex->int x)])

;; Character classes, there's a lot to unpack
(defmethod transform [:BCC :common] [[_ x]] (transform x))
(defmethod transform [:BCCIntersection :common] [[_ x]] (transform x))
(defmethod transform [:BCCUnionLeft :common] [[_ & xs]]
  (if (= (ffirst xs) :BCCNegation)
    (into [:not] (comp (map transform) collapse-strings-xf) (next xs))
    (into [:class] (comp (map transform) collapse-strings-xf) xs)))

(defmethod transform [:BCCElemHardLeft :common] [[_ x]] (transform x))
(defmethod transform [:BCCElemNonLeft :common] [[_ x]] (transform x))
(defmethod transform [:BCCElemBase :common] [[_ x]] (transform x))
(defmethod transform [:BCCCharNonRange :common] [[_ x]] (transform x))
(defmethod transform [:BCCCharEndRange :common] [[_ x]] (transform x))
(defmethod transform [:BCCChar :common] [[_ x]] (transform x))
(defmethod transform [:BCCPlainChar :common] [[_ x]] (transform x))
(defmethod transform [:BCCRange :common] [[_ x y]]
  [(transform x) (transform y)])

(defmethod transform [:GroupFlags :common] [[_ x]]
  (case (first x)
    :NamedCapturingGroup
    (throw (ex-info "Not implemented" {:feature (first x)}))
    :NonCapturingMatchFlags
    [:non-capturing-group (transform x)]
    :PositiveLookAheadFlag
    (throw (ex-info "Not implemented" {:feature (first x)}))
    :NegativeLookAheadFlag
    (throw (ex-info "Not implemented" {:feature (first x)}))
    :PositiveLookBehindFlag
    (throw (ex-info "Not implemented" {:feature (first x)}))
    :NegativeLookBehindFlag
    (throw (ex-info "Not implemented" {:feature (first x)}))
    :IndependentNonCapturingFlag
    (throw (ex-info "Not implemented" {:feature (first x)}))))

(defmethod transform [:NonCapturingMatchFlags :common] [[_ x]] (transform x))
(defmethod transform [:MatchFlagsExpr :common] [[_ & xs]] xs)
(defmethod transform [:NonCapturingGroup :common] [[_ x]] (transform x))

(defmethod transform [:ParenthesizedExpr :common] [[_ x y]]
  (if y
    (let [[type flags] (transform x)
          form (transform y)]
      (case type
        :non-capturing-group
        (with-meta (if (string? form)
                     [:cat form]
                     form)
          {::flags flags})))
    [:capture (transform x)]))

(defn parse [pattern]
  (->> pattern
       remove-QE
       (instaparse/parse @parser)
       transform))

(comment
  (for [p ["[x]" "[xy]" "[x-y]" "[^x-y]" "[x-y_]"]]
    [p (parse p)])

  [(parse "(x)")
   (parse "(?:x)")
   ;;(parse "(?<name>x)")
   ;;(parse "(?idmsuxU)x")

   (parse "(?-idmsuxU:x)")]
  (require 'lambdaisland.regal.test-util)
  (for [{:keys [cases]} (lambdaisland.regal.test-util/test-cases)
        {:keys [pattern form]} cases]
    (if (map? pattern)
      (for [[flavor pattern] pattern]
        [flavor (regal/with-flavor flavor [form (parse pattern)])])
      [form (parse pattern)])
    )

  (instaparse/parse @parser "a")

  (run! regal/regex )
  (prn (s/gen :lambdaisland.regal/form))

  (regal/regex [:cat "abc"])
  (transform (instaparse/parse @parser "(?:x|yz)"))

  (parse "\\u000D\\u000A|[\\u000A\\u000B\\u000C\\u000D\\u0085\\u2028\\u2029]"))
