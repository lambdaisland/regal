(ns lambdaisland.regal.spec-alpha
  (:require [lambdaisland.regal :as regal]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(s/fdef regal/regex :args (s/cat :form ::regal/form))

(s/def ::regal/form
  (s/or :literal ::regal/literal
        :token   ::regal/token
        :op      ::regal/op))

(s/def ::regal/literal
  (s/or :string string?
        :char   char?))

(s/def ::regal/token (set (keys (deref (var regal/tokens)))))

(defmulti op first)

(s/def ::regal/-op
  (s/multi-spec op (fn [v t]
                     (into [t] (rest v)))))

(s/def ::regal/op
  (s/with-gen (s/and vector? ::regal/-op)
    #(gen/fmap vec (s/gen ::regal/-op))))

(defn op-spec [args-spec]
  (s/cat :op keyword?
         :args args-spec))

(defmethod op :cat [_]
  (op-spec (s/* ::regal/form)))

(defmethod op :alt [_]
  (op-spec (s/* ::regal/form)))

(defmethod op :capture [_]
  (op-spec (s/* ::regal/form)))

(defmethod op :* [_]
  (op-spec (s/+ ::regal/form)))

(defmethod op :+ [_]
  (op-spec (s/+ ::regal/form)))

(defmethod op :? [_]
  (op-spec (s/+ ::regal/form)))

(s/def ::regal/repeat-min nat-int?)
(s/def ::regal/repeat-max nat-int?)

(defmethod op :repeat [_]
  (op-spec (s/cat :form ::regal/form
                  :min  ::regal/repeat-min
                  :min  (s/? ::regal/repeat-max))))

(s/def ::regal/range-start char?)
(s/def ::regal/range-end char?)

(s/def ::regal/range
  (s/cat :start ::regal/range-start
         :end   ::regal/range-end))

(defmethod op :range [_]
  (op-spec ::regal/range))

(s/def ::regal/class
  (s/+ (s/or :range ::regal/range
             :char   char?
             :string string?)))

(defmethod op :class [_]
  (op-spec ::regal/class))

(defmethod op :not [_]
  (op-spec ::regal/class))
