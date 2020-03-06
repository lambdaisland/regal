(require '[lambdaisland.regal.spec-alpha :as regal-spec]
         '[lambdaisland.regal :as regal]
         '[lambdaisland.regal.parse :as parse]
         '[clojure.test.check.generators]
         '[clojure.spec.gen.alpha :as gen]
         '[clojure.spec.alpha :as s]
         '[instaparse.core :as instaparse])

(->> "abc"
     #_remove-QE
     (instaparse/parse parser)
     )

(instaparse/transform
 {:Regex (fn [r] r)
  :Alternation (fn [& alts]
                 (if (= (count alts) 1)
                   (first alts)
                   (into [:alt] (remove nil?) alts)))
  :Concatenation (fn [& cats]
                   (if (= (count cats) 1)
                     (first cats)
                     (into [:cat] (remove nil?) cats)))
  :DanglingCurlyRepetitions (fn [& reps])
  :SuffixedExpr (fn
                  ([expr]
                   expr)
                  ([expr suffix]
                   (throw (ex-info [:SuffixedExpr expr suffix]))))
  :SingleExpr identity
  :BaseExpr identity
  :LiteralChar identity
  :PlainChar identity}
 (->> "ab\\x01"
      #_remove-QE
      (instaparse/parse @parse/parser)
      )

 )


(run! regal/regex )
(prn (s/gen :lambdaisland.regal/form))

(regal/regex [:cat "abc"])
