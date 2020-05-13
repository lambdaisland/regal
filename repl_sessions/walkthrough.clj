(ns walkthrough
  (:require [lambdaisland.regal :as regal]
            [lambdaisland.regal.parse :as parse]
            [lambdaisland.regal.generator :as generator]))


(def name-regex #"([A-Z][a-z]+)\s+([A-Z][a-z]+)")

(re-find name-regex "Arne Brasseur")
;; => ["Arne Brasseur" "Arne" "Brasseur"]

(def regal-form (parse/parse name-regex))
;;=>
[:cat
 [:capture
  [:cat
   [:class [\A \Z]]
   [:+ [:class [\a \z]]]]]
 [:+ :whitespace]
 [:capture
  [:cat
   [:class [\A \Z]]
   [:+ [:class [\a \z]]]]]]

(regal/with-flavor :ecma
  (regal/regex regal-form))
;; => #"([A-Z][a-z]+)\s+([A-Z][a-z]+)"

(regal/with-flavor :java9
  (regal/regex regal-form))
;; => #"([A-Z][a-z]+)[\u0009\u000A\u000B\u000C\u000D\u0020\u00A0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u2028\u2029\u202F\u205F\u3000]+([A-Z][a-z]+)"

(generator/sample regal-form)
;; => ("Ys Lh"
;;     "Mi Bw"
;;     "Rfe  Xfog"
;;     "Njh   Gai"
;;     "Uw \n\r Yp"
;;     "Km　\t Uh"
;;     "Zbmsngx Ohwtpks"
;;     "Zliusncy\n   Uvboxry"
;;     "Dwmneiy\fZfajiq"
;;     "Iqvh \t     Sfixk")

;; - regal form
;; - regular expression -> java8
;; - regular expression -> java9
;; - regular expression -> javascript
;; - string -> match result
;; - generated values

REGAL FORM:

[:cat
 [:capture
  [:cat
   [:class [\A \Z]]
   [:+ [:class [\a \z]]]]]
 [:+ :whitespace]
 [:capture
  [:cat
   [:class [\A \Z]]
   [:+ [:class [\a \z]]]]]]

FLAVOR: [*] Java 8   [*] Java 9    [*] JavaScript

REGEX: #"([A-Z][a-z]+)\s+([A-Z][a-z]+)"

INPUT STRING: Arne Brasseur

RESULT: ["Arne Brasseur" "Arne" "Brasseur"]

GENERATED: [Update]
'("Ys Lh"
  "Mi Bw"
  "Rfe  Xfog"
  "Njh   Gai"
  "Uw \n\r Yp"
  "Km　\t Uh"
  "Zbmsngx Ohwtpks"
  "Zliusncy\n   Uvboxry"
  "Dwmneiy\fZfajiq"
  "Iqvh \t     Sfixk")
