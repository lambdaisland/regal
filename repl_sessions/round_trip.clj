(ns round-trip
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [lambdaisland.regal :as regal]
            [lambdaisland.regal.parse :as parse]
            [lambdaisland.regal.spec-alpha :as rs]
            [lambdaisland.regal.platform :as platform]))

(gen/sample
 (s/gen :lambdaisland.regal/form))

(defn round-trip? [form]
  (= form (parse/parse (regal/regex form))))

(check/quick-check
 100
 (prop/for-all*
  [(gen/fmap regal/normalize (s/gen :lambdaisland.regal/form))]
  round-trip?))

(check/quick-check
 100
 (prop/for-all [regal (s/gen ::regal/form)]
   ;; test that generators generate valid regexes
   (try
     (regal/regex regal)
     (catch Exception _
       false))))

(round-trip? [:cat "   " [:class "&& "]])


(round-trip? [:class " " [" " "["]])

(round-trip? [:ctrl "A"])

(round-trip? [:class "   - "])

(round-trip? [:alt "  " [:capture " " :escape]])

(round-trip? :whitespace)

(round-trip? [:? [:? "x"]])

(round-trip? [:cat "  " [:class " " :non-whitespace]])

(round-trip? [:cat "-" [:repeat [:repeat "x" 0] 0]])

(dotimes [i 100]
  (print i)
  (time (doall (map round-trip? (gen/sample (s/gen ::regal/form) i)))))

(re-find
 #"-(?:x{2})?"
 "-x"

 )

(re-find #"ax??" "ax")

(re-find #"[ ^]" "^")
(re-find #"[$]" "$")
(re-find #"[.]" "a")
(re-find #"[a\-z]" "b")
(re-find #"[a\-z]" "-")

(regal/pattern [:not " " :whitespace])

(regal/compile "[\\R]")

(parse/parse
 (regal/regex [:* " " :escape]))
;; => [:* [:cat " " :escape]]
