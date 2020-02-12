(ns lambdaisland.regal
  "Compile Regal syntax to regex patterns.

     >>> (compile-str [:cat
                       :start
                       [:class [\\a \\z] [\\A \\Z] [\\0 \\9] \\_ \\-]
                       \"@\"
                       [:repeat [:range \\0 \\9] 3 5]
                       [:* [:not \\.]]
                       [:alt \"com\" \"org\" \"net\"]
                       :end])
     \"\\\\A[a-zA-Z0-9_-]\\\\Q@\\\\E[0-9]{3,5}([^.]*)(\\\\Qcom\\\\E|\\\\Qorg\\\\E|\\\\Qnet\\\\E)\\\\Z\" "
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
   :any "."})

(declare regal->grouped)

(defmulti -regal->grouped first)

(defmethod -regal->grouped :cat [[_ & rs]]
  (map regal->grouped rs))

(defmethod -regal->grouped :alt [[_ & rs]]
  (interpose \| (map regal->grouped rs)))

(defmethod -regal->grouped :* [[_ r]]
  (list (regal->grouped r) \*))

(defmethod -regal->grouped :+ [[_ r]]
  (list (regal->grouped r) \+))

(defmethod -regal->grouped :? [[_ & rs]]
  (list (regal->grouped (into [:cat] rs)) \?))

(defmethod -regal->grouped :range [[_ from to]]
  `^::grouped (\[ ~from \- ~to \]))

(defn- compile-class [cs]
  (reduce (fn [r c]
            (cond
              (string? c)
              (conj r c)

              (char? c)
              (conj r c)

              (vector? c)
              (conj r (first c) "-" (second c))))
          []
          cs))

(defmethod -regal->grouped :class [[_ & cs]]
  `^::grouped (\[ ~@(compile-class cs) \]))

(defmethod -regal->grouped :not [[_ & cs]]
  `^::grouped (\[ \^ ~@(compile-class cs) \]))

(defmethod -regal->grouped :repeat [[_ r & ns]]
  `^::grouped (~(regal->grouped r) \{ ~@(interpose \, (map str ns)) \} ))

(defn- regal->grouped [r]
  (cond
    (string? r)
    (regex-escape r)

    (char? r)
    (regex-escape (str r))

    (keyword? r)
    (get tokens r)

    :else
    (-regal->grouped r)))

(defn- simplify [g]
  (if (or (string? g) (char? g) (::grouped (meta g)))
    g
    (let [g (map simplify g)]
      (if (and (= 1 (count g))
               (string? (first g)))
        (first g)
        g))))

(defn- grouped->str* [g]
  (cond
    (or (string? g) (char? g))
    g

    (or (seq? g) (vector? g))
    (let [s (apply str (map grouped->str* g))]
      (if (::grouped (meta g))
        s
        (str "(" s ")")))

    :else
    (assert false g)))

(defn- grouped->str [g]
  (apply str (map grouped->str* g)))

(defn- compile-str [r]
  (-> r
      regal->grouped
      simplify
      grouped->str))

(defn regex [r]
  (make-regex (compile-str r)))
