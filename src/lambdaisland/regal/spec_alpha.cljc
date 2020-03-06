(ns lambdaisland.regal.spec-alpha
  (:require [lambdaisland.regal :as regal]
            [lambdaisland.regal.generator :as generator]
            [clojure.test.check.generators :as gen]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as spec-gen]))

;; (s/fdef regal/regex :args (s/cat :form ::regal/form
;;                                  :options (s/o)))


(s/def ::regal/form
  (s/or :literal ::regal/literal
        :token   ::regal/token
        :op      ::regal/op))

(s/def ::regal/literal
  (s/or :string ::non-blank-string
        :char   char?))

(s/def ::non-blank-string
  (s/with-gen
    (s/and string?
           #(pos? (count %)))
    (fn []
      (gen/bind
       (gen/sized #(gen/choose 1 (inc %)))
       (fn [size]
         gen/string)))))

(s/def ::regal/token (->> regal/token->ir
                          methods
                          keys
                          (filter vector?)
                          (map first)
                          set))


(defmulti op first)

(s/def ::regal/-op
  (s/multi-spec op (fn [v t]
                     (into [t] (rest v)))))

(s/def ::regal/op
  (s/with-gen (s/and vector? ::regal/-op)
    #(spec-gen/fmap vec (s/gen ::regal/-op))))

(defn op-spec [args-spec]
  (s/cat :op keyword?
         :args args-spec))

(defmethod op :cat [_]
  (op-spec (s/+ ::regal/form)))

(defmethod op :alt [_]
  (op-spec (s/cat :form ::regal/form
                  :rest (s/+ ::regal/form))))

(defmethod op :capture [_]
  (op-spec (s/+ ::regal/form)))

(defmethod op :* [_]
  (op-spec (s/+ ::regal/form)))

(defmethod op :+ [_]
  (op-spec (s/+ ::regal/form)))

(defmethod op :? [_]
  (op-spec (s/+ ::regal/form)))

(s/def ::regal/repeat-min nat-int?)
(s/def ::regal/repeat-max nat-int?)

(s/def ::regal/repeat-impl
  (s/and
   (op-spec (s/cat :form ::regal/form
                   :min  ::regal/repeat-min
                   :max  (s/? ::regal/repeat-max)))
   (fn [{{:keys [_ min max]} :args}]
     (or (nil? max) (< min max)))))

(def form-gen (s/gen ::regal/form))

(defmethod op :repeat [_]
  (s/with-gen
    ::regal/repeat-impl
    (fn []
      (gen/fmap (fn [[form & minmax]]
                  (into [form] (sort minmax)))
                (gen/tuple form-gen gen/nat gen/nat)))))

(s/def ::regal/class
  (s/+ (s/or :char   char?
             :string ::non-blank-string)))

(defmethod op :class [_]
  (op-spec ::regal/class))

(defmethod op :not [_]
  (op-spec ::regal/class))

(defn- resolver [kw]
  (-> kw s/spec meta ::form))

(defn spec [regal]
  (let [opts    {:resolver resolver}
        pattern (regal/regex regal opts)]
    (with-meta
      (s/with-gen
        (partial re-find pattern)
        #(generator/gen regal opts))
      {::form regal})))

(comment

  (s/valid? :lambdaisland.regal/form [:cat [:+ "x"] "-" [:+ "y"]])
  ;; => true

  (s/conform :lambdaisland.regal/form [:cat [:+ "x"] "-" [:+ "y"]])
  ;; => [:op
  ;;     {:op :cat,
  ;;      :args
  ;;      [[:op {:op :+, :args [[:literal [:string "x"]]]}]
  ;;       [:literal [:string "-"]]
  ;;       [:op {:op :+, :args [[:literal [:string "y"]]]}]]}]

  )
