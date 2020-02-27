(ns lambdaisland.regal.parse
  (:require [instaparse.core :as instaparse]
            #?(:clj [clojure.java.io :as io]))
  #?(:cljs
     (:require-macros [lambdaisland.regal.parse :refer [inline-resource]])))

#?(:clj
   (defmacro inline-resource [path]
     (slurp (io/resource path))))

(def grammar
  #?(:clj (io/resource "lambdaisland/regal/regex.bnf")
     :cljs (inline-resource "lambdaisland/regal/regex.bnf")))

(def parser (delay (instaparse/parser grammar)))

(defn ^:private remove-QE
  "Preprocesses a regex string (the same way that openjdk does) by
  transforming all \\Q...\\E expressions. Returns a string which is
  an equivalent regex that contains no \\Q...\\E expressions."
  [^String s]
  (if (.contains s "\\Q")
    (letfn [(remove-QE-not-quoting [chars]
              (lazy-seq
               (when-let [[c1 & [c2 :as cs]] (seq chars)]
                 (if (= \\ c1)
                   (if (= \Q c2)
                     (remove-QE-quoting-init (rest cs))
                     (list* c1 c2 (remove-QE-not-quoting (rest cs))))
                   (cons c1 (remove-QE-not-quoting cs))))))
            ;; I don't understand why this clause in the java code,
            ;; but it is, and this is what it does, and it has some
            ;; weird effects, like in #"\c\Q0"
            (remove-QE-quoting-init [chars]
              (when-let [[c1 :as cs] (seq chars)]
                (if (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} c1)
                  (list* \\ \x \3 c1
                         (remove-QE-quoting (rest cs)))
                  (remove-QE-quoting cs))))
            (remove-QE-quoting [chars]
              (lazy-seq
               (when-let [[c1 & [c2 :as cs]] (seq chars)]
                 (if (and (= c1 \\) (= c2 \E))
                   (remove-QE-not-quoting (rest cs))
                   (if (or (re-matches #"[0-9a-zA-Z]" (str c1))
                           (<= 128 (int c1) ))
                     (cons c1 (remove-QE-quoting cs))
                     (list* \\ c1 (remove-QE-quoting cs)))))))]
      (apply str (remove-QE-not-quoting s)))
    s))
