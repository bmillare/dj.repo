(ns dj.html
  (:require [dj.sgml]))

(defn html5 [content]
  [{:!tag :DOCTYPE
    :attrs "html"}
   {:tag :html
    :attrs {:lang "en"}
    :content content}])

(defn base [content]
  (html5 (into [{:tag :head
                 :content [{:tag :meta
                            :attrs {:charset "UTF-8"}}]}]
               content)))