(ns lambdaisland.regal.platform
  (:require [clojure.string :as str])) ;; cljs

(defn hex->int [hex]
  (js/parseInt hex 16))

(defn int->hex [i]
  (str/upper-case
   (.toString i 16)))

(defn char->long [ch]
  (.charCodeAt ^String ch))

(defn parse-int [i]
  (js/parseInt i 10))
