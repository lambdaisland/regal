(ns lambdaisland.regal.generator
  (:require [lambdaisland.regal.pattern :as pattern]
            [clojure.test.check.generators :as gen]))

(declare generator)

(defmulti -generator first)

(defmethod -generator :cat [[_ & rs]]
  (apply gen/tuple (map generator rs)))

(defmethod -generator :alt [[_ & rs]]
  (gen/one-of (map generator rs)))

(defmethod -generator :* [[_ r]]
  (gen/bind gen/pos-int
            (fn [i]
              (apply gen/tuple (repeat i (generator r))))))

(defmethod -generator :+ [[_ r]]
  (gen/bind gen/s-pos-int
            (fn [i]
              (apply gen/tuple (repeat i (generator r))))))

(defmethod -generator :? [[_ & rs]]
  (gen/one-of [(gen/return "")
               (generator (into [:cat] rs))]))

(defn char-code [s]
  #?(:clj
     (cond
       (string? s) (.charCodeAt ^String s 0)
       (char? s)   (long s)
       :else       (assert false s))
     :cljs
     (.charCodeAt s 0)))

(defmethod -generator :range [[_ from to]]
  (gen/fmap char (gen/choose (char-code from) (char-code to))))

(defmethod -generator :class [[_ & cs]]
  (gen/one-of (for [c cs]
                (if (vector? c)
                  (gen/fmap char (gen/choose (char-code (first c)) (char-code (second c))))
                  (gen/return c)))))

(defmethod -generator :not [r]
  ;; TODO: this is a bit hacky
  (let [pattern (pattern/regex r)]
    (gen/such-that #(re-find pattern (str %)) gen/char)))

(defmethod -generator :repeat [[_ r min max]]
  (gen/bind (gen/choose min max)
            (fn [i]
              (apply gen/tuple (repeat i (generator r))))))

(defn generator [r]
  (cond
    (string? r)
    (gen/return r)

    (char? r)
    (gen/return r)

    (keyword? r)
    (case r
      :any gen/char
      (gen/return ""))

    :else
    (-generator r)))

(defn grouped->str
  #?@(:clj
      [([g]
        (let [sb (StringBuilder.)]
          (run! #(grouped->str % sb) g)
          (str sb)))
       ([g ^StringBuilder sb]
        (cond
          (string? g)
          (.append sb ^String g)

          (char? g)
          (.append sb ^Character g)

          (or (seq? g) (vector? g))
          (run! #(grouped->str % sb) g)

          :else
          (assert false g)))]
      :cljs
      [([g]
        (cond
          (string? g)
          g
          (or (seq? g) (vector? g))
          (apply str (map grouped->str g))
          :else
          (assert false g)))]))

(defn gen [r]
  (gen/fmap grouped->str (generator r)))

(defn sample [r]
  (gen/sample (gen r)))

(comment
  (regal/sample
   [:cat
    :start
    [:class [\a \z] [\A \Z] [\0 \9] \_ \-]
    "@"
    [:repeat [:range \0 \9] 3 5]
    [:* [:not \.]]
    [:alt "com" "org" "net"]
    :end])

  (def r [:cat [:+ [:range \a \z]] "=" [:+ [:not "="]]])
  (sample r)
  ("t=" "d=5Ë" "zja=·" "uatt=ß¾" "lqyk=É" "xkj=q\f" "gxupw=æ" "pkadbgmc=¯²" "f=ÃJ" "d=ç")
  )

(gen/sample gen/char)



    (require '[lambdaisland.regal :as regal])

    ;; Regal expression, like Hiccup but for Regex
    (def r [:cat
            [:+ [:range \a \z]]
            "="
            [:+ [:not \=]]])

    ;; Match values...
    (regal/regex r)
    ;;=> #"([a-z]+)=([^=]+)"

    (re-matches (regal/regex r) "foo=bar")
    ;;=> ["foo=bar" "foo" "bar"]

    ;; ... And generate them
    (regal/gen r)
    ;;=> #clojure.test.check.generators.Generator{...}

    (regal/sample r)
    ;;=> ("t=" "d=5Ë" "zja=·" "uatt=ß¾" "lqyk=É" "xkj=q\f" "gxupw=æ" "pkadbgmc=¯²" "f=ÃJ" "d=ç")
