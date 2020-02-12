(ns lambdaisland.regal.generator
  (:require [lambdaisland.regal :as regal]
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
  (let [pattern (regal/regex r)]
    (gen/such-that #(re-find pattern (str %)) gen/char)))

(defmethod -generator :repeat [[_ r min max]]
  (gen/bind (gen/choose min max)
            (fn [i]
              (apply gen/tuple (repeat i (generator r))))))

(defmethod -generator :capture [[_ & rs]]
  (generator (into [:cat] rs)))

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
  (let [pattern [:cat
                 :start
                 [:class [\a \z] [\A \Z] [\0 \9] \_ \-]
                 "@"
                 [:capture
                  [:repeat [:range \0 \9] 3 5]
                  [:* [:not \.]]
                  "."
                  [:alt "com" "org" "net"]]
                 :end]]
    (map #(re-find (regal/regex pattern) %) (sample pattern))))
