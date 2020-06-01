(ns lambdaisland.regal.spec-alpha
  (:require [lambdaisland.regal :as regal]
            [lambdaisland.regal.generator :as generator]
            [clojure.test.check.generators :as gen]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as spec-gen]
            [clojure.string :as str]))

;; (s/fdef regal/regex :args (s/cat :form ::regal/form
;;                                  :options (s/o)))

(s/def ::regal/form
  (s/with-gen
    (s/or :literal ::regal/literal
          :token   ::regal/token
          :op      ::regal/op)
    ;; Bit of a hack, we need to stop test.check from blowing up the stack by
    ;; making sure these recursive forms at some point bottom out. At smaller
    ;; sizes we emit token/literals which never recur, at larger size we reduce
    ;; the size before recurring so that we bottom out at a predicatable depth.
    (fn []
      (gen/sized
       (fn [size]
         (cond
           (< size 5)
           (s/gen ::regal/token)
           (< size 10)
           (s/gen (s/or :literal ::regal/literal
                        :token ::regal/token))
           :else
           (gen/resize
            (dec (long (* size 3/4)))
            (s/gen ::regal/op))))))))

(s/def ::regal/literal
  (s/or :string ::non-blank-string
        :char   (s/with-gen char?
                  (constantly gen/char-ascii))))

(s/def ::non-blank-string
  (s/with-gen
    (s/and string?
           #(pos? (count %)))
    (fn []
      (gen/bind
       (gen/sized #(gen/choose 1 (inc %)))
       (fn [size]
         (gen/fmap #(apply str %) (gen/vector gen/char-ascii size)))))))

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

(defn op-spec [op args-spec]
  (s/cat :op #{op}
         :args args-spec))

(defmethod op :cat [_]
  (op-spec :cat (s/+ ::regal/form)))

(defmethod op :alt [_]
  (op-spec :alt (s/cat :form ::regal/form
                       :rest (s/+ ::regal/form))))

(defmethod op :capture [_]
  (op-spec :capture (s/+ ::regal/form)))

(defmethod op :* [_]
  (op-spec :* (s/+ ::regal/form)))

(defmethod op :+ [_]
  (op-spec :+ (s/+ ::regal/form)))

(defmethod op :? [_]
  (op-spec :? (s/+ ::regal/form)))

(s/def ::regal/repeat-min nat-int?)
(s/def ::regal/repeat-max nat-int?)

(s/def ::regal/repeat-impl
  (s/and
   (op-spec :repeat (s/cat :form ::regal/form
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
                  (let [[min max] (sort minmax)]
                    (if (= min max)
                      [:repeat form min]
                      [:repeat form min max])))
                (gen/tuple form-gen gen/nat gen/nat)))))

(s/def ::single-character
  (s/or :char (s/with-gen char?
                (constantly gen/char-ascii))
        :string (s/with-gen (s/and string? #(= (count %) 1))
                  #(gen/fmap str gen/char-ascii))))

(s/def ::regal/range (s/with-gen (s/cat :from ::single-character
                                        :to ::single-character)
                       #(gen/tuple (s/gen ::single-character)
                                   (s/gen ::single-character))))

(s/def ::regal/class
  (s/+ (s/or :char   (s/with-gen char?
                       (constantly gen/char-ascii))
             :range  ::regal/range
             :string ::non-blank-string
             :token  ::regal/token)))


(defmethod op :class [_]
  (op-spec :class ::regal/class))

(defmethod op :not [_]
  (op-spec :not ::regal/class))

(defmethod op :ctrl [_]
  (op-spec :ctrl (s/cat :char char?)))

(defn- resolver [kw]
  (-> kw s/spec meta ::form))

(defn spec
  "Turn a regal form into a clojure.spec.alpha spec

     (s/def ::spaces (lambdaisland.regal.spec-alpha/spec [:+ :whitespace]))
  "
  [regal]
  (let [opts    {:resolver resolver}
        pattern (regal/regex regal opts)]
    (with-meta
      (s/with-gen
        (partial re-find pattern)
        #(generator/gen regal opts))
      {::form regal})))

(comment

  (gen/generate (s/gen ::regal/form) 100)

  (binding [s/*recursion-limit* 1]
    (spec-gen/generate (s/gen ::regal/-op)))

  (s/valid? ::regal/repeat-impl
            (doto (spec-gen/generate (gen/fmap (fn [[form & minmax]]
                                                 (into [:repeat form] (sort minmax)))
                                               (gen/tuple form-gen gen/nat gen/nat)))
              prn))

  (s/valid? ::regal/repeat-impl [:repeat :word 2 2])

  (s/valid? ::regal/op [:ctrl \u0230])

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
