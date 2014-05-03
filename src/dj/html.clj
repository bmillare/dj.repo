(ns dj.html
  (:require [dj.sgml]))

(defn html5 [content]
  [[:!DOCTYPE "html"]
   (apply vector :html {:lang "en"} content)])

#_ (defn base [content]
  (html5 (into [{:tag :head
                 :content [{:tag :meta
                            :attrs {:charset "UTF-8"}}]}]
               content)))

(def basic
  [[:!DOCTYPE "html"]
   [:html {:lang "en"}
    [:head nil
     [:meta {:charset "UTF-8"}]]]])

