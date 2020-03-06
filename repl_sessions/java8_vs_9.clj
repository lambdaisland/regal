(ns java8-vs-9
  (:require [lambdaisland.regal :as regal]))

;; https://stackoverflow.com/questions/47871962/why-does-r-behave-differently-in-regular-expressions-between-java-8-and-java-9
;; https://stackoverflow.com/questions/42474596/java-8-regex-negative-lookbehind-with-r
;; https://bugs.openjdk.java.net/browse/JDK-8176029


;; The unicode recommendation is that \R matches any one of \r, \n, vertical
;; tab, next line, line separator, or paragraph separator, OR it can match \r\n
;; as a unit. Unicode also explicitly state that there should be no backtracking
;; in the middle of an \r\n pair, i.e. \R\R should match \n\n, but not \r\n.
;;
;; Java 8 did this correctly, but documented it incorrectly. In Java 9 the
;; implementation has been updated, rather than the documentation. So Java
;; 9 (and later) are basically "wrong".
;;
;; ECMAScript does not support \R
;;
;; Unicode helpfully has an equivalent pattern that behaves as it should, so we
;; can emulate the "right" behavior in Java 9 and later as well as ECMAScript.
;;
;; http://unicode.org/reports/tr18/#Line_Boundaries

(def R-regex #"(?:\r\n|(?!\r\n)[\n-\r\u0085\u2028\u2029])")

(regal/pattern R-regex)
"(?:\\r\\n|(?!\\r\\n)[\\n-\\r\\u0085\\u2028\\u2029])"
