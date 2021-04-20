(ns lambdaisland.regal.generator
  (:require [clojure.test.check.generators :as gen]
            [lambdaisland.regal :as regal]
            [lambdaisland.regal.platform :as platform]
            [clojure.string :as str]))

(declare generator)

(defmulti -generator (fn [[op] opts] op))

(defn collapse-double-line-breaks [rs]
  (let [[res prev] (reduce
                    (fn [[res prev] r]
                      (cond
                        (= prev r :line-break)
                        [(conj res :-double-line-break) nil]
                        prev
                        [(conj res prev) r]
                        :else
                        [res r]
                        )
                      )
                    [[] nil]
                    rs)]
    (if prev
      (conj res prev)
      res)))

(defn map-generator [rs opts]
  (map-indexed (fn [idx r]
                 (generator r (-> opts
                                  (update ::initial? #(and % (= 0 idx)))
                                  (update ::final? #(and % (= (count rs)
                                                              (inc idx))))))) rs))

(defmethod -generator :cat [[_ & rs] opts]
  (apply gen/tuple (map-generator (collapse-double-line-breaks rs) opts)))


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

(defn parse-hex
  "
  \"\\xFF\" => 255
  \"\\u0A00\" => 2560
  "
  [h]
  (platform/hex->int (subs h 2)))

(def any-gen
  (gen/such-that (complement #{\r \n \u0085 "\r" "\n" "\u0085"}) gen/char))

(def whitespace-gen
  (gen/fmap (comp char parse-hex) (gen/one-of (map gen/return regal/whitespace-chars))))

(def non-whitespace-gen
  (gen/fmap
   char
   (gen/one-of
    (map
     (comp
      (fn [[from to]]
        (gen/choose (parse-hex from)
                    (parse-hex to)))
      #(str/split % #"-"))
     regal/non-whitespace-ranges))))

(def line-break-gen
  (gen/one-of (map gen/return ["\r\n" "\n" "\u000B" "\f" "\r" "\u0085" "\u2028" "\u2029"])))

(def double-line-break-gen
  "[:cat :line-break :line-break] should not generate \\r\\n, because of how \\R
  works."
  (gen/such-that
   (complement #{"\r\n"})
   (gen/fmap #(apply str %)
             (gen/tuple line-break-gen line-break-gen))))

(defn token-gen [r opts]
  (case r
    :any
    any-gen ;; . does not match newlines

    :digit
    (-generator [:class [\0 \9]] opts)

    :non-digit
    (-generator [:not [\0 \9]] opts)

    :word
    (-generator [:class [\a \z] [\A \Z] [\0 \9] \_] opts)

    :non-word
    (-generator [:not [\a \z] [\A \Z] [\0 \9] \_] opts)

    :whitespace
    whitespace-gen

    :non-whitespace
    non-whitespace-gen

    :start
    (if (::initial? opts)
      (gen/return "")
      (throw (ex-info "Can't create generator, :start used in non-initial position."
                      {:type ::impossible-regex})))

    :end
    (if (::final? opts)
      (gen/return "")
      (throw (ex-info "Can't create generator, :end used in non-final position."
                      {:type ::impossible-regex})))

    :newline
    (gen/return "\n")

    :return
    (gen/return "\r")

    :tab
    (gen/return "\t")

    :form-feed
    (gen/return "\f")

    :line-break
    line-break-gen

    :-double-line-break ;; internal, do not use
    double-line-break-gen

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
                  (gen/fmap char (gen/choose (platform/char->long (first c)) (platform/char->long (second c))))

                  (simple-keyword? c)
                  (token-gen c opts)

                  ;; Not sure if this should be allowed, can custom tokens be
                  ;; used inside a class?
                  ;;
                  ;; (qualified-keyword? c)
                  ;; (generator c opts)

                  (string? c)
                  (gen/one-of (map gen/return c))

                  (char? c)
                  (gen/return c)))))

(defmethod -generator :not [r opts]
  ;; TODO: this is a bit hacky
  (let [pattern (regal/regex r opts)]
    (gen/such-that #(re-find pattern (str %)) gen/char)))

(defmethod -generator :repeat [[_ r min max] opts]
  (if max
    (gen/bind (gen/choose min max)
              (fn [i]
                (apply gen/tuple (repeat i (generator r opts)))))
    (apply gen/tuple (repeat min (generator r opts)))))

(defmethod -generator :capture [[_ & rs] opts]
  (generator (into [:cat] rs) opts))

(defmethod -generator :ctrl [[_ ch] opts]
  (gen/return (str (char (- (platform/char->long
                             (if (char? ch)
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
   (gen/fmap grouped->str (generator r (assoc opts
                                              ::initial? true
                                              ::final? true)))))

(defn sample [r]
  (gen/sample (gen r)))

(comment
  (sample [:cat :digit :whitespace :word])

  (sample [:class "aeiou" :whitespace])

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
