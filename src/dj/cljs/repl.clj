;;; DEPRECATED
;;; This file is for reference only, see scratch/gallook/[repl,devmode,cider-cljs,
(ns dj.cljs.repl
  (:refer-clojure :exclude [eval])
  (:require [cljs.repl]
            [cljs.env]
            [cljs.analyzer]))

(defn eval [repl-env form]
  (cljs.env/with-compiler-env
    (:cljs.env/compiler repl-env)
    (cljs.repl/evaluate-form
     repl-env
     (assoc (cljs.analyzer/empty-env)
       :ns (cljs.analyzer/get-namespace cljs.analyzer/*cljs-ns*))
     "<dj.cljs/cljs-eval>"
     form)))
