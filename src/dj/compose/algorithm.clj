(ns dj.compose.algorithm
  (:require [clojure.set :as cs]))

;; Utilities for computing dependencies

(defn reverse-dag
  "returns a dag but directions are now reversed"
  [dag]
  (reduce-kv (fn [d k children]
	       (reduce (fn [d' child]
			 (update-in d'
				    [child]
				    (fn [parents]
				      (if (empty? parents)
					#{k}
					(conj parents k)))))
		       d
		       children))
	     {}
	     dag))

(defn topological-sort
  "dag is a map from nodes->set of nodes of children, this outputs the
  nodes children first"
  [dag]
  (let [nodes (set (keys dag))
	;; need this to efficiently compute parents
	rdag (reverse-dag dag)
	root-nodes (reduce-kv (fn [s k v]
				(cs/difference s v))
			      nodes
			      dag)]
    (loop [s root-nodes
	   out ()
	   rd rdag]
      (if (empty? s)
	(if (every? empty? (vals rd))
	  out
	  (throw (Exception. "cyclic dependencies")))
	(let [n (first s)
	      children (dag n)
	      new-out (list* n out)
	      new-rd (reduce (fn [rd' m]
			       (update-in rd'
					  [m]
					  disj
					  n))
			     rd
			     children)
	      new-s (cs/union (disj s n)
                              (set
                               (filter #(empty?
                                         (new-rd %))
                                       children)))]
	  (recur new-s new-out new-rd))))))
