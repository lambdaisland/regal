(ns install-expound
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]))

(set! s/*explain-out* expound/printer)
