(ns lambdaisland.regal.generator
  (:require [lambdaisland.regal :as regal]
            [clojure.test.check.generators :as gen]))

(declare generator)

(defmulti -generator (fn [[op] opts] op))

(defmethod -generator :cat [[_ & rs] opts]
  (apply gen/tuple (map #(generator % opts) rs)))

(defmethod -generator :alt [[_ & rs] opts]
  (gen/one-of (map #(generator % opts) rs)))

(defmethod -generator :* [[_ & rs] opts]
  (gen/bind gen/pos-int
            (fn [i]
              (apply gen/tuple (repeat i (generator (into [:cat] rs) opts))))))

(defmethod -generator :+ [[_ & rs] opts]
  (gen/bind gen/s-pos-int
            (fn [i]
              (apply gen/tuple (repeat i (generator (into [:cat] rs) opts))))))

(defmethod -generator :? [[_ & rs] opts]
  (gen/one-of [(gen/return "")
               (generator (into [:cat] rs) opts)]))

(defn- char-code [s]
  #?(:clj
     (cond
       (string? s) (.charCodeAt ^String s 0)
       (char? s)   (long s)
       :else       (assert false s))
     :cljs
     (.charCodeAt s 0)))

(defn token-gen [r opts]
  (case r
    :any
    gen/char

    :digit
    (recur [:class [\0 \9]] opts)

    :non-digit
    (recur [:not [\0 \9]] opts)

    :word
    (recur [:class [\a \z] [\A \Z] [\0 \9] \_] opts)

    :non-word
    (recur [:not [\a \z] [\A \Z] [\0 \9] \_] opts)

    :whitespace
    (recur [:class \space \tab \newline \u000B \formfeed \return] opts)

    :non-whitespace
    (recur [:not \space \tab \newline \u000B \formfeed \return] opts)

    :start
    (gen/return "")

    :end
    (gen/return "")

    :newline
    (gen/return "\n")

    :return
    (gen/return "\r")

    :tab
    (gen/return "\t")

    :form-feed
    (gen/return "\f")

    :line-break
    (gen/one-of (map gen/return ["\r\n" "\n" "\u000B" "\f" "\r" "\u0085" "\u2028" "\u2029"]))

    :alert
    (gen/return "\u0007")

    :escape
    (gen/return "\u001B")

    :vertical-whitespace
    (gen/one-of (map gen/return ["\n" "\u000B" "\f" "\r" "\u0085" "\u2028" "\u2029"]))

    :vertical-tab
    (gen/return "\u000B")

    :null
    (gen/return "\u0000")

    (throw (ex-info (str "Unrecognized regal token: " r) {::unrecognized-token r}))))

(defmethod -generator :class [[_ & cs] opts]
  (gen/one-of (for [c cs]
                (cond
                  (vector? c)
                  (gen/fmap char (gen/choose (char-code (first c)) (char-code (second c))))

                  (simple-keyword? c)
                  (token-gen c opts)

                  (or (string? c) (char? c))
                  (gen/return c)))))

(defmethod -generator :not [r opts]
  ;; TODO: this is a bit hacky
  (let [pattern (regal/regex r opts)]
    (gen/such-that #(re-find pattern (str %)) gen/char)))

(defmethod -generator :repeat [[_ r min max] opts]
  (gen/bind (gen/choose min max)
            (fn [i]
              (apply gen/tuple (repeat i (generator r opts))))))

(defmethod -generator :capture [[_ & rs] opts]
  (generator (into [:cat] rs) opts))

(defmethod -generator :ctrl [[_ ch] opts]
  (gen/return (str (char (- (long (if (char? ch)
                                    ch
                                    (first ch)))
                            64)))))


(defn- generator [r {:keys [resolver] :as opts}]
  (cond
    (string? r)
    (gen/return r)

    (char? r)
    (gen/return r)

    (qualified-keyword? r)
    (if resolver
      (if-let [resolved (resolver r)]
        (recur resolved opts)
        (throw (ex-info (str "Unable to resolve Regal Expression " r ".")
                        {::unresolved r})))
      (throw (ex-info (str "Regal expression contains qualified keyword, but no resolver was specified.")
                      {::no-resolver-for r})))

    (simple-keyword? r)
    (token-gen r opts)

    :else
    (-generator r opts)))

(defn- grouped->str
  #?@(:clj
      [([g]
        (let [sb (StringBuilder.)]
          (grouped->str g sb)
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

(defn gen
  ([r]
   (gen r nil))
  ([r {:keys [resolver] :as opts}]
   (gen/fmap grouped->str (generator r opts))))

(defn sample [r]
  (gen/sample (gen r)))

(comment
  (sample [:cat :digit :whitespace :word])

  (sample
   [:cat
    :start
    [:alt "http" "https" "ftp"]
    "://"
    [:+ [:+ :word] "."]
    [:+ :word]
    [:? [:+ "/" [:+ [:not "/?#"]]]]
    [:? "?" [:+ [:+ :word] "=" [:+ :word]]
     [:? [:+  "&" [:+ :word] "=" [:+ :word]]]]])

  (require '[lambdaisland.regal :as regal])
  (sample [:cat
           :start
           [:+ :word]
           "="
           [:+ :digit]
           :end])

  (let [pattern [:cat
                 :start
                 [:class [\a \z] [\A \Z] [\0 \9] \_ \-]

                 [:capture
                  [:repeat [:class [\0 \9]] 3 5]
                  [:* [:not \.]]
                  "."
                  [:alt "com" "org" "net"]]
                 :end]]
    (map #(re-find (regal/regex pattern) %) (sample pattern))))
