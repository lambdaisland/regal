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
  #?(:clj (:import java.util.regex.Pattern)))

;; - Do we need escaping inside [:class]? caret/dash?

#?(:clj (set! *warn-on-reflection* true))

(defn- regex-escape [s]
  #?(:clj
     (Pattern/quote s)
     ;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#Escaping
     :cljs
     (.replace s (js* "/[.*+?^${}()|[\\]\\\\]/g") "\\\\$&")))

(defn- make-regex [s]
  #?(:clj
     (Pattern/compile s)
     :cljs
     (js/RegExp. s)))

(def ^:private tokens
  {:start #?(:clj "\\A" :cljs "^")
   :end #?(:clj "\\z" :cljs "$")
   :any "."
   :digit "\\d"
   :non-digit "\\D"
   :word "\\w"
   :non-word "\\W"
   :whitespace "\\s"
   :non-whitespace "\\S"})

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

(declare regal->ir)

(defmulti -regal->ir (fn [[op] opts] op))

(defmethod -regal->ir :cat [[_ & rs] opts]
  (map #(regal->ir % opts) rs))

(defmethod -regal->ir :alt [[_ & rs] opts]
  (interpose \| (map #(regal->ir % opts) rs)))

(defn quantifier->ir [q rs opts]
  (let [rsg (regal->ir (into [:cat] rs) opts)
        ;; This forces explicit grouping for multi-character string
        ;; literals so that e.g. [:* "ab"] compiles to #"(?:ab)*"
        ;; rather than #"ab*".
        rsg (if (and (string? rsg)
                     (not (next rs))
                     (> (count (str (first rs))) 1))
              (list rsg)
              rsg)]
    `^::grouped (~rsg ~q)))

(defmethod -regal->ir :* [[_ & rs] opts]
  (quantifier->ir \* rs opts))

(defmethod -regal->ir :+ [[_ & rs] opts]
  (quantifier->ir \+ rs opts))

(defmethod -regal->ir :? [[_ & rs] opts]
  (quantifier->ir \? rs opts))

(defmethod -regal->ir :repeat [[_ r & ns] opts]
  (quantifier->ir `^::grouped (\{ ~@(interpose \, (map str ns)) \}) [r] opts))

(defn- compile-class [cs]
  (reduce (fn [r c]
            (cond
              (string? c)
              (conj r c)

              (char? c)
              (conj r c)

              (vector? c)
              (conj r (first c) \- (second c))))
          []
          cs))

(defmethod -regal->ir :class [[_ & cs] opts]
  `^::grouped (\[ ~@(compile-class cs) \]))

(defmethod -regal->ir :not [[_ & cs] opts]
  `^::grouped (\[ \^ ~@(compile-class cs) \]))

(defmethod -regal->ir :capture [[_ & rs] opts]
  `^::grouped (\( ~@(regal->ir (into [:cat] rs) opts) \)))

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
    (regex-escape r)

    (char? r)
    (regex-escape (str r))

    (qualified-keyword? r)
    (if resolver
      (if-let [resolved (resolver r)]
        (recur resolved opts)
        (throw (ex-info (str "Unable to resolve Regal Expression " r ".")
                        {::unresolved r})))
      (throw (ex-info (str "Regal expression contains qualified keyword, but no resolver was specified.")
                      {::no-resolver-for r})))

    (simple-keyword? r)
    (get tokens r)

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
    (assert false g)))

(defn- grouped->str [g]
  (apply str (map grouped->str* g)))

(defn- compile-str [r opts]
  (-> r
      (regal->ir opts)
      grouped->str))

(defn regex
  "Convert a Regal Expression into a platform-specific regex pattern object.

  Can take an options map:

  - `:resolver` a function/map used to resolve namespaced keywords inside Regal
  expressions.
  "
  ([r]
   (regex r nil))
  ([r {:keys [resolver] :as opts}]
   (make-regex (compile-str r opts))))
