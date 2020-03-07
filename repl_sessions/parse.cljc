(require '[lambdaisland.regal :as regal]
         '[lambdaisland.regal.parse :as regal-parse])

;; \R is a line break, it matches \r\n, \r, \n, and some other unicode line
;; breaks. The corresponding Regal form for this is :line-break
(regal/with-flavor :java8 (regal-parse/parse #"\R"))
;; => :line-break

;; Java 9 has different semantics than :line-breaks, so we return a Regal form
;; that emulates Java 9 semantics.
(regal/with-flavor :java9 (regal-parse/parse #"\R"))
;; => [:alt
;;     [:cat [:char 13] [:char 10]]
;;     [:class
;;      [:char 10]
;;      [:char 11]
;;      [:char 12]
;;      [:char 13]
;;      [:char 133]
;;      [:char 8232]
;;      [:char 8233]]]

;; JavaScript doesn't have \R
(regal/with-flavor :ecma (regal-parse/parse-pattern "\\R"))
;; => ExceptionInfo
;; #:lambdaisland.regal.parse{:not-supported
;;                            {:flavor :ecma,
;;                             :feature :line-break,
;;                             :pattern "\\R"}}

;; So if you use :line-break we emit a regex with the same semantics as \R
(regal/with-flavor :ecma (regal/pattern :line-break))
;; => "(?:\\r\\n|(?!\\r\\n)[\\n-\\r\\x85\\u2028\\u2029])"

;; Clojure
(regal/regex :vertical-whitespace) ;;=> #"\v"

;; ClojureScript
(regal/regex :vertical-whitespace) ;;=> #"[\n\x0B\f\r\x85\u2028\u2029]"


(regal/regex-pattern #"\u000B\v") ;; => "\\u000B\\v"
