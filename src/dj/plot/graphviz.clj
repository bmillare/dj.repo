(ns dj.plot.graphviz
  (:require [clojure.java.shell :as cjs]))

;; we prefer to work with clojure data not some DSL

;; each edge is represented as:
;; a pair vector of ids (keywords) from first to second
;; or for undirected, an pair set of ids

;; attributes

;; attribute map, hashmap of attribute-key -> attribute-value

;; node attributes
;; hashmap from id -> attribute-map

;; edge attributes
;; hashmap of vector/set pairs -> attribute-map

;; graph map

;; graph map has attributes, type (digraph or graph), and edges

#_ [{:type :graph
     :name :graphname
     :attributes {:graph {:size "1,1"}
                  :node {:a {:label "Foo"}
                         :b {:shape :box}}
                  :edge {#{:a :b} {:color :blue}
                         #{:b :c} {:color :blue}
                         #{:b :d} {:style :dotted}}}
     :edges #{#{:a :b}
              #{:b :c}
              #{:b :d}}}
    {:type :digraph
     :name :graphname
     :attributes {:graph {:size "1,1"}
                  :node {:a {:label "Foo"}
                         :b {:shape :box}}
                  :edge {[:a :b] {:color :blue}
                         [:b :c] {:color :blue}
                         [:b :d] {:style :dotted}}}
     :edges #{[:a :b]
              [:b :c]
              [:b :d]}}]

(defn emit-graph-attributes [attributes]
  (apply str
         (for [[k v] attributes]
           (str (name k)
                "="
                (if (keyword? v)
                  (name v)
                  (str "\"" v "\""))
                ";"))))

(defn emit-vector-attributes [attributes]
  (str (apply str
              "["
              (interpose " "
                         (for [[k v] attributes]
                           (str (name k)
                                "="
                                (if (keyword? v)
                                  (name v)
                                  (str "\"" v "\""))))))
       "]"))

(defn graphviz
  "{:type :graph
     :name :graphname
     :attributes {:graph {:size \"1,1\"}
                  :node {:a {:label \"Foo\"}
                         :b {:shape :box}}
                  :edge {#{:a :b} {:color :blue}
                         #{:b :c} {:color :blue}
                         #{:b :d} {:style :dotted}}}
     :edges #{#{:a :b}
              #{:b :c}
              #{:b :d}}}
    {:type :digraph
     :name :graphname
     :attributes {:graph {:size \"1,1\"}
                  :node {:a {:label \"Foo\"}
                         :b {:shape :box}}
                  :edge {[:a :b] {:color :blue}
                         [:b :c] {:color :blue}
                         [:b :d] {:style :dotted}}}
     :edges #{[:a :b]
              [:b :c]
              [:b :d]}}"
  [graph]
  (let [{:keys [type name attributes edges]} graph
        edge-seperator ({:digraph "->"
                         :graph "--"} type)]
    (str (clojure.core/name type) " " (clojure.core/name name) "{"
         (emit-graph-attributes (:graph attributes))
         (apply str (for [[nid attrs] (:node attributes)]
                      (str "\"" (clojure.core/name nid) "\"" (emit-vector-attributes attrs) ";")))
         (apply str (for [e edges]
                      (str "\"" (clojure.core/name (first e)) "\""
                           edge-seperator
                           "\"" (clojure.core/name (second e)) "\""
                           (if-let [edge (:edge attributes)]
                             (emit-vector-attributes (edge e)))
                           ";")))
         "}")))

(defn emit
  "See graphivz"
  [graph]
  (:out (cjs/sh "neato" "-Tsvg" :in (graphviz graph))))
