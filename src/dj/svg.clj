(ns dj.svg
  (:require [dj.sgml]
            [clojure.string :as cs]))

(defn emit-points
  "converts point data into svg formatted point data"
  [x y]
  (cs/join " "
           (map (fn [xi yi]
                  (str xi "," yi))
                x
                y)))

(defrecord points [x y]
  dj.sgml/IEmitForm
  (-emit-form [_]
    (emit-points x y)))


(defn svg [attrs content]
  {:tag :svg
   :attrs (merge {:xmlns "http://www.w3.org/2000/svg"
                  :version "1.1"}
                 attrs)
   :content content})

(defn emit-transform-seq [transform-seq]
  (cs/join " "
           (for [[op & args] transform-seq]
             (str (name op) "(" (cs/join "," args) ")"))))

(defrecord ts [transform-seq]
  dj.sgml/IEmitForm
  (-emit-form [_]
    (emit-transform-seq transform-seq)))