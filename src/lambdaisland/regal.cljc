(ns lambdaisland.regal
  "Compile Regal syntax to regex patterns.

     >>> (regex [:cat
                 :start
                 [:class [\\a \\z] [\\A \\Z] [\\0 \\9] \\_ \\-]
                 \"@\"
                 [:repeat [:range \\0 \\9] 3 5]
                 [:* [:not \\.]]
                 [:alt \"com\" \"org\" \"net\"]
                 :end])
     #\"\\A[a-zA-Z0-9_-]\\Q@\\E[0-9]{3,5}[^.]*(?:\\Qcom\\E|\\Qorg\\E|\\Qnet\\E)\\z\" "
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set])
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

;; TODO: Turn into extensible registry
(def ^:private aliases
  {:start #?(:clj "\\A" :cljs "^")
   :end #?(:clj "\\z" :cljs "$")
   :any "."})

(s/def ::literal
  (s/conformer (fn [x]
                 {:literal x})
               :literal))

(s/def ::alias
  (s/conformer (fn [x]
                 {:alias x})
               :alias))

(defn renaming-conformer [kmap]
  (let [ikmap (set/map-invert kmap)]
    (s/conformer (fn [x]
                   (set/rename-keys x kmap))
                 (fn [x]
                   (set/rename-keys x ikmap)))))

(defmulti op :op)
(defmulti grouped? :op)

(defmethod grouped? :default [_] false)

(defmethod op :cat [_]
  (s/and (renaming-conformer {:args :forms})
         (s/keys :req-un [::forms])))

(defmethod op :alt [_]
  (s/and (renaming-conformer {:args :forms})
         (s/keys :req-un [::forms])))

(defmethod op :capture [_]
  (s/and (renaming-conformer {:args :forms})
         (s/keys :req-un [::forms])))

(defmethod grouped? :capture [_] true)

(defmethod op :* [_]
  (s/and (renaming-conformer {:args :forms})
         (s/keys :req-un [::forms])))

(defmethod op :+ [_]
  (s/and (renaming-conformer {:args :forms})
         (s/keys :req-un [::forms])))

(defmethod op :? [_]
  (s/and (renaming-conformer {:args :forms})
         (s/keys :req-un [::forms])))

(s/def ::min nat-int?)
(s/def ::max nat-int?)

(s/def ::repeat
  (s/and (s/cat :form any?
                :min  any?
                :max  (s/? any?))
         (s/keys :req-un [::form ::min]
                 :opt-un [::max])))

(defmethod op :repeat [_]
  (s/and (renaming-conformer {:args :repeat})
         (s/keys :req-un [::repeat])))


(s/def ::range-start char?)
(s/def ::range-end char?)

(s/def ::range
  (s/and (s/cat :range-start any?
                :range-end   any?)
         (s/keys :req-un [::range-start ::range-end])))

(defmethod op :range [_]
  (s/and (renaming-conformer {:args :range})
         (s/keys :req-un [::range])))

(defmethod grouped? :range [_] true)

(s/def ::class
  (s/+ (s/or :range ::range
             :char   char?
             :string string?)))

(defmethod op :class [_]
  (s/and (renaming-conformer {:args :class})
         (s/keys :req-un [::class])))

(defmethod grouped? :class [_] true)

(defmethod op :not [_]
  (s/and (renaming-conformer {:args :class})
         (s/keys :req-un [::class])))

(defmethod grouped? :not [_] true)

(s/def ::op
  (s/multi-spec op (fn [v t] (cons t v))))

(s/def ::forms
  (s/coll-of ::form))

(s/def ::form
  (s/and (s/or :literal (s/and string?  ::literal)
               :literal (s/and char?    ::literal)
               :alias   (s/and keyword? ::alias)
               :op      (s/and (s/coll-of any? :into [])
                               (s/cat :op keyword? :args (s/* any?))
                               ::op))
         (s/conformer (fn [[form x]]
                        (assoc x :form form))
                      (fn [x]
                        [(:form x) (dissoc x :form)]))))

(declare form->regex)

(defmulti -form->regex :form)

(defmethod -form->regex :literal [form]
  (regex-escape (str (:literal form))))

(defmethod -form->regex :alias [form]
  (get aliases (:alias form)))

(defmulti op->regex :op)

(defmethod op->regex :cat [{:keys [forms]}]
  ;; TODO: Join contiguous runs of literals
  (if (next forms)
    (map form->regex forms)
    (form->regex (first forms)
                 {:grouping :skip})))

(defmethod op->regex :alt [op]
  (interpose \| (map form->regex (:forms op))))

(defmethod op->regex :capture [op]
  (list \(
        (form->regex (assoc op :op :cat)
                     {:grouping :skip})
        \)))

(defn literal? [form]
  (= :literal (:form form)))

(defn suffix-op->regex [forms op]
  (let [fform (first forms)]
    (if (and (nil? (next forms))
             (if (literal? fform)
               (or (char? (:literal fform))
                   (= 1 (count (:literal fform))))
               (grouped? fform)))
      (list (form->regex fform) op)
      (list (form->regex {:form :op
                          :op :cat
                          :forms forms}
                         {:grouping :force})
            op))))

(defmethod op->regex :* [{:keys [forms]}]
  (suffix-op->regex forms \*))

(defmethod op->regex :+ [{:keys [forms]}]
  (suffix-op->regex forms \+))

(defmethod op->regex :? [{:keys [forms]}]
  (suffix-op->regex forms \?))

(defmethod op->regex :repeat [op]
  (let [{:keys [form min max]} (:repeat op)]
    (suffix-op->regex [form]
                      (if (= min max)
                        (str "{" min "}")
                        (str "{" min "," max "}")))))

(defmethod op->regex :class [op]
  `(\[
    ~(when (:complement? op)
       \^)
    ~@(mapcat (fn [[t x]]
                (if (= :range t)
                  [(:range-start x) \- (:range-end x)]
                  [x]))
              (:class op))
    \]))

(defmethod op->regex :range [op]
  (op->regex {:form :op
              :op :class
              :class [[:range (:range op)]]}))

(defmethod op->regex :not [op]
  (op->regex (assoc op
                    :op :class
                    :complement? true)))

(defmethod -form->regex :op [form]
  (op->regex form))

(defn regex-str-group [regex-str]
  (str "(?:" regex-str ")"))

(defn form->regex
  ([form]
   (form->regex form {}))
  ([form {:keys [grouping]}]
   (let [regex     (-form->regex form)
         regex-str (if (string? regex)
                     regex
                     (apply str regex))]
     (cond (= :force grouping)
           (regex-str-group regex-str)

           (string? regex)
           regex-str

           (= :skip grouping)
           regex-str

           (grouped? form)
           regex-str

           (next regex)
           (regex-str-group regex-str)

           :else
           regex-str))))

(defn regex [r]
  (s/assert ::form r)
  (let [form (s/conform ::form r)]
    (when-not (s/invalid? form)
      (make-regex (form->regex form {:grouping :skip})))))
