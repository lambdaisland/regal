(ns lambdaisland.regal
  "Compile Regal syntax to regex patterns.

     >>> (regex [:cat
                 :start
                 [:class [\\a \\z] [\\A \\Z] [\\0 \\9] \\_ \\-]
                 \"@\"
                 [:repeat [:class [\\0 \\9]] 3 5]
                 [:* [:not \\.]]
                 [:alt \"com\" \"org\" \"net\"]
                 :end])
     #\"\\A[a-zA-Z0-9_-]\\Q@\\E[0-9]{3,5}[^.]*(?:\\Qcom\\E|\\Qorg\\E|\\Qnet\\E)\\z\" "
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
            [lambdaisland.regal.platform :as platform])
  #?(:clj (:import java.util.regex.Pattern)
     :cljs (:require-macros [lambdaisland.regal :refer [with-flavor]])))

;; - Do we need escaping inside [:class]? caret/dash?

#?(:clj (set! *warn-on-reflection* true))

(def flavor-hierarchy (-> (make-hierarchy)
                          (derive :java :common)
                          (derive :ecma :common)
                          (derive :java8 :java)   ; = Java 8
                          (derive :java9 :java))) ; >= Java 9

(defn runtime-flavor
  "The regex flavor that the current runtime understands."
  []
  #?(:clj (let [version (re-find #"\A\d+" (System/getProperty "java.version"))]
            (if (= "1" version) ;; 1.8 vs 9 / 11
              :java8
              :java9))
     :cljs :ecma))

(def ^:dynamic *flavor* (runtime-flavor))
(def ^:dynamic *character-class* false)

(defn current-flavor? [f]
  (isa? flavor-hierarchy *flavor* f))

#?(:clj
   (defmacro with-flavor
     "Set the flavor of regex to use for generating and parsing regex patterns.
  Defaults to the flavor understood by the runtime. `flavor` can be `:ecma`,
  `:java8` (Java 8) or `:java9` (Java 9 or later). Earlier Java versions are not
  officially supported, meaning some patterns will behave differently."
     [flavor & body]
     `(binding [*flavor* ~flavor]
        ~@body)))

(defn escape
  "Escape a regex pattern, so that, when compiled to a regex object, it will match
  all characters literally."
  [s]
  ;; The replacement string backreference escaping seems to work differently
  ;; in clj vs cljs, this may be a ClojureScript bug
  (str/replace s #"([.*+?^${}()|\[\]\\])" #?(:clj "\\\\$1" :cljs "\\$1")))

(defn regex-pattern
  "Regex to string, remove the slashes that JavaScript likes to add. This will
  drop any regex modifiers."
  [r]
  #?(:clj (str r)
     :cljs (let [s (str r)
                 ;; there may be modifiers after the last slash
                 len (.lastIndexOf s "/")]
             (-> s
                 (.substring 1 len)
                 ;; Undo escaping of forward slashes, in JS this is necessary to
                 ;; distinguish them from regex boundaries, but for us they are
                 ;; irrelevant
                 (str/replace "\\/" "/")))))

(defn compile
  "Compile a regex pattern (string) to a regex."
  [s]
  #?(:clj
     (Pattern/compile s)
     :cljs
     (js/RegExp. s)))

;; IR = Intermediate Representation
;;
;; Instead of going directly from Regal Expression to regex pattern we first
;; convert to an intermediate form, consisting of potentially nested lists of
;; strings, which when concatenated yield a regex pattern.
;;
;; Each list is conceptually grouped, and is typically converted into a
;; non-capturing regex group in the final conversion. If a given list represents
;; a regex pattern that is already treated as a single entity e.g. by
;; quantifiers, then the list is given the metadata `{::grouped true}`

(def whitespace-chars
  "These are characters with the Unicode whitespace property. In JavaScript these
  are all matched by \\s, except for NEXT LINE and MONGOLIAN VOWEL SEPARATOR. In
  Java \\s only matches the ASCII one. In Regal :whitespace emulates the
  JavaScript semantics of \\s."
  ^::grouped
  ["\\u0009" ;; CHARACTER TABULATION
   "\\u000A" ;; LINE FEED (LF)
   "\\u000B" ;; LINE TABULATION
   "\\u000C" ;; FORM FEED (FF)
   "\\u000D" ;; CARRIAGE RETURN (CR)
   "\\u0020" ;; SPACE
   ;; "\\u0085" ;; NEXT LINE (NEL)
   "\\u00A0" ;; NO-BREAK SPACE
   "\\u1680" ;; OGHAM SPACE MARK
   ;; "\\u180E" ;; MONGOLIAN VOWEL SEPARATOR
   "\\u2000" ;; EN QUAD
   "\\u2001" ;; EM QUAD
   "\\u2002" ;; EN SPACE
   "\\u2003" ;; EM SPACE
   "\\u2004" ;; THREE-PER-EM SPACE
   "\\u2005" ;; FOUR-PER-EM SPACE
   "\\u2006" ;; SIX-PER-EM SPACE
   "\\u2007" ;; FIGURE SPACE
   "\\u2008" ;; PUNCTUATION SPACE
   "\\u2009" ;; THIN SPACE
   "\\u200A" ;; HAIR SPACE
   "\\u2028" ;; LINE SEPARATOR
   "\\u2029" ;; PARAGRAPH SEPARATOR
   "\\u202F" ;; NARROW NO-BREAK SPACE
   "\\u205F" ;; MEDIUM MATHEMATICAL SPACE
   "\\u3000"]) ;; IDEOGRAPHIC SPACE

(def non-whitespace-ranges
  "Character ranges that are not whitespace (the opposite of the above)"
  ^::grouped
  ["\\x00-\\x08"
   "\\x0E-\\x1F"
   "\\x21-\\x9F"
   "\\xA1-\\u167F"
   "\\u1681-\\u1FFF"
   "\\u200B-\\u2027"
   "\\u202A-\\u202E"
   "\\u2030-\\u205E"
   "\\u2060-\\u2FFF"
   "\\u3001-\\uFFFF"])

(defmulti token->ir (fn [token] [token *flavor*]) :hierarchy #'flavor-hierarchy)

(defmethod token->ir :default [token]
  (throw (ex-info (str "Unrecognized token: " token)
                  {::unrecognized-token token
                   ::flavor *flavor*})))

(defmethod token->ir [:start :common] [_] "^")
(defmethod token->ir [:end :common] [_] "$")
(defmethod token->ir [:any :common] [_] ".")
(defmethod token->ir [:digit :common] [_] "\\d")
(defmethod token->ir [:non-digit :common] [_] "\\D")
(defmethod token->ir [:word :common] [_] "\\w")
(defmethod token->ir [:non-word :common] [_] "\\W")
(defmethod token->ir [:newline :common] [_] "\\n")
(defmethod token->ir [:return :common] [_] "\\r")
(defmethod token->ir [:tab :common] [_] "\\t")
(defmethod token->ir [:form-feed :common] [_] "\\f")

(defn assert-line-break-not-in-class []
  ;; Java does not allow #"[\R]", and emulating the behaviour of \R inside a
  ;; class is not possible either, so we don't support it.
  (when *character-class*
    (throw (java.lang.UnsupportedOperationException. ":line-break can not be used inside [:class] or [:not]"))))

(defmethod token->ir [:line-break :java8] [_]
  (assert-line-break-not-in-class)
  "\\R")

(defmethod token->ir [:line-break :java9] [_]
  (assert-line-break-not-in-class)
  "(?:\\r\\n|(?!\\r\\n)[\\n-\\r\\x85\\u2028\\u2029])")

(defmethod token->ir [:line-break :ecma] [_]
  (assert-line-break-not-in-class)
  "(?:\\r\\n|(?!\\r\\n)[\\n-\\r\\x85\\u2028\\u2029])")

(defmethod token->ir [:alert :java] [_] "\\a")
(defmethod token->ir [:alert :ecma] [_] "\\x07")

(defmethod token->ir [:escape :java] [_] "\\e")
(defmethod token->ir [:escape :ecma] [_] "\\x1B")

(defmethod token->ir [:vertical-whitespace :java] [_] "\\v")
(defmethod token->ir [:vertical-whitespace :ecma] [_]
  (if *character-class*
    "\\n\\x0B\\f\\r\\x85\\u2028\\u2029"
    "[\\n\\x0B\\f\\r\\x85\\u2028\\u2029]"))

(defmethod token->ir [:whitespace :java] [_]
  (if *character-class*
    whitespace-chars
    `^::grouped (\[ ~whitespace-chars \])))

(defmethod token->ir [:non-whitespace :java] [_]
  (if *character-class*
    ;; if we're part of a bigger character class then emulate non-whitespace by
    ;; including ranges of characters that are not whitespace between
    ;; Character/MIN_VALUE and Character/MAX_VALUE
    non-whitespace-ranges
    `^::grouped (\[ \^ ~whitespace-chars \])))

(defmethod token->ir [:whitespace :ecma] [_] "\\s")
(defmethod token->ir [:non-whitespace :ecma] [_] "\\S")

(defmethod token->ir [:vertical-tab :java] [_] "\\x0B")
(defmethod token->ir [:vertical-tab :ecma] [_] "\\v")

(defmethod token->ir [:null :java] [_] "\\x00")
(defmethod token->ir [:null :ecma] [_] "\\0")

(declare regal->ir)

(defmulti -regal->ir (fn [[op] opts] [op *flavor*]) :hierarchy #'flavor-hierarchy)

(defmethod -regal->ir [:cat :common] [[_ & rs] opts]
  (map #(regal->ir % opts) rs))

(defmethod -regal->ir [:alt :common] [[_ & rs] opts]
  (interpose \| (map #(regal->ir % opts) rs)))

;; Still missing a few like \u{xxx}
(defn single-character? [s]
  (when (string? s)
    (case (count s)
      1
      true

      2
      (or (= s "\\\\")
          (re-find #"\\0[0-7]" s)
          (re-find #"\\[trnfaedDsSwW]" s))

      3
      (or (re-find #"\\0[0-7]{2}" s)
          (re-find #"\\x[0-9a-zA-Z]{2}" s)
          (re-find #"\\c[A-Z]" s))

      4
      (re-find #"\\0[0-3][0-7]{2}" s)

      5
      (if (current-flavor? :java)
        (or (when-let [[_ match] (re-find #"\\Q(.*)\\E" s)]
              (single-character? match))
            (re-find #"\\u[0-9a-zA-Z]{4}" s))
        s)

      false)))

(defn quantifier->ir [q rs opts]
  (let [rsg (regal->ir (into [:cat] rs) opts)
        ;; This forces explicit grouping for multi-character string
        ;; literals so that e.g. [:* "ab"] compiles to #"(?:ab)*"
        ;; rather than #"ab*".
        rsg (if (or (seq? rsg) (single-character? rsg))
              rsg
              (list rsg))]
    `^::grouped (~rsg ~q)))

(defmethod -regal->ir [:* :common] [[_ & rs] opts]
  (quantifier->ir \* rs opts))

(defmethod -regal->ir [:+ :common] [[_ & rs] opts]
  (quantifier->ir \+ rs opts))

(defmethod -regal->ir [:? :common] [[_ & rs] opts]
  (quantifier->ir \? rs opts))

(defmethod -regal->ir [:repeat :common] [[_ r & ns] opts]
  (quantifier->ir `^::grouped (\{ ~@(interpose \, (map str ns)) \}) [r] opts))

(defn char-class-escape [ch]
  (case ch
    \^
    "\\^"
    \]
    "\\]"
    ;; unescaped opening brackets are in fact allowed inside character classes.
    ;; In JavaScript this allows nesting, in Java it matches a literal opening
    ;; bracket. Escaped it works the same on both.
    \[
    "\\["
    \\
    "\\\\"
    ch))

(defn- compile-class [cs]
  (reduce (fn [r c]
            (cond
              (string? c)
              (into r (map char-class-escape) c)

              (char? c)
              (conj r (char-class-escape c))

              (vector? c)
              (if (#{:char :ctrl} (first c))
                (conj r (regal->ir c {}))
                (conj r (first c) \- (second c)))

              (keyword? c)
              (conj r (token->ir c))))
          []
          cs))

(defmethod -regal->ir [:class :common] [[_ & cs] opts]
  (binding [*character-class* true]
    `^::grouped (\[ ~@(compile-class cs) \])))

(defmethod -regal->ir [:not :common] [[_ & cs] opts]
  (binding [*character-class* true]
    `^::grouped (\[ \^ ~@(compile-class cs) \])))

(defmethod -regal->ir [:capture :common] [[_ & rs] opts]
  `^::grouped (\( ~@(regal->ir (into [:cat] rs) opts) \)))

(defmethod -regal->ir [:lookahead :common] [[_ & rs] opts]
  `^::grouped (\( \? \= ~@(regal->ir (into [:cat] rs) opts) \)))

(defmethod -regal->ir [:negative-lookahead :common] [[_ & rs] opts]
  `^::grouped (\( \? \! ~@(regal->ir (into [:cat] rs) opts) \)))

(defmethod -regal->ir [:lookbehind :common] [[_ & rs] opts]
  `^::grouped (\( \? \< \= ~@(regal->ir (into [:cat] rs) opts) \)))

(defmethod -regal->ir [:negative-lookbehind :common] [[_ & rs] opts]
  `^::grouped (\( \? \< \! ~@(regal->ir (into [:cat] rs) opts) \)))

(defmethod -regal->ir [:atomic :common] [[_ & rs] opts]
  `^::grouped (\( \? \> ~@(regal->ir (into [:cat] rs) opts) \)))

(defn left-pad [s len pad]
  (with-meta
    (concat (repeat (- len (count s)) pad)
            s)
    {::grouped true}))

(defmethod -regal->ir [:char :common] [[_ ch] opts]
  {:pre [(int? ch)]}
  (if (< ch 256)
    `^::grouped (\\ \x ~(left-pad (platform/int->hex ch) 2 \0))
    `^::grouped (\\ \u ~(left-pad (platform/int->hex ch) 4 \0))))

(defmethod -regal->ir [:ctrl :common] [[_ ch] opts]
  (let [ch (if (string? ch) (first ch) ch)]
    (assert (<= (platform/char->long \A) (platform/char->long ch) (platform/char->long \Z)))
    `^::grouped (\\ \c ~ch)))

(defn- regal->ir
  "Convert a Regal expression into an intermediate representation,
  consisting of strings and nested lists, which when all concatenated together
  yield a regex string.

  A list can have the `::grouped` metadata, which indicates the regex it
  contains naturally introduces some kind of grouping.


      >>> (regal->ir \"hello\" {})
      \"\\Qhello\\E\"

      >>> (regal->ir [:cat \"foo\" \"bar\"] {})
      (\"\\Qfoo\\E\" \"\\Qbar\\E\")

      >>> (regal->ir [:class [\"a\" \"z\"]] {})
      ^::grouped (\\[ \"a\" \\- \"z\" \\])"
  [r {:keys [resolver] :as opts}]
  (cond
    (string? r)
    (escape r)

    (char? r)
    (escape (str r))

    (qualified-keyword? r)
    (if resolver
      (if-let [resolved (resolver r)]
        (recur resolved opts)
        (throw (ex-info (str "Unable to resolve Regal Expression " r ".")
                        {::unresolved r})))
      (throw (ex-info (str "Regal expression contains qualified keyword, but no resolver was specified.")
                      {::no-resolver-for r})))

    (simple-keyword? r)
    (token->ir r)

    :else
    (let [g (-regal->ir r opts)]
      (if (or (::grouped (meta g)) (next g))
        g
        (first g)))))

(defn- grouped->str* [g]
  (cond
    (or (string? g) (char? g))
    g

    (or (seq? g) (vector? g))
    (let [s (apply str (map grouped->str* g))]
      (if (::grouped (meta g))
        s
        (str "(?:" s ")")))

    :else
    (throw (ex-info (str "Unrecognized component: " g)
                    {::unrecognized-component g}))))

(defn- grouped->str [g]
  (apply str (map grouped->str* g)))

(defn pattern
  "Convert a Regal form to a regex pattern as a string."
  ([form]
   (pattern form nil))
  ([form opts]
   (-> form
       (regal->ir opts)
       grouped->str)))

(defn regex
  "Convert a Regal form into a platform-specific regex pattern object.

  Can take an options map:

  - `:resolver` a function/map used to resolve namespaced keywords inside Regal
  expressions.
  "
  ([form]
   (regex form nil))
  ([form {:keys [resolver] :as opts}]
   (compile (pattern form opts))))



(comment

  (regex [:class :any])

  (with-flavor :ecma
    (pattern [:class :line-break "xyz"])
    )
  )
