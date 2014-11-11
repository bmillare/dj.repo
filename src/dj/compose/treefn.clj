(ns dj.compose.treefn
  (:refer-clojure :exclude [fn])
  (:require [clojure.set :as cs]
            [dj.compose.algorithm :as dca]))

(defmacro fn
  "adds dependency metadata to fn

  Use this only with treefn"
  [bindings & body]
  `(with-meta (clojure.core/fn ~bindings
                ~@body)
     {:dj.compose {:dependencies '~(mapv keyword bindings)}}))

(defmacro fm
  "adds dependency metadata to fn, uses map destructuring
assume you provide the vector in {:keys [a b ...]}

  Use this only with treefm
  "
  [bindings & body]
  `(with-meta (clojure.core/fn [{:keys ~bindings}]
                ~@body)
     {:dj.compose {:dependencies '~(mapv keyword bindings)}}))

(defmacro wrapfn
  "
Convenience macro:

adds dependency metadata to an existing fn or var

Takes a fn-or-var and a vector of keys
"
  [fn-or-var vec-of-keys]
  `(with-meta ~fn-or-var
     {:dj.compose {:dependencies ~vec-of-keys}}))

(declare ^:dynamic eval-pass)

;; fastest, fully inlined, best for inner loops, generation time argument checking
(defn inlined-emitter [shaken-map
                    symbols
                    shaken-keys
                    input-keys
                    sorted-keys
                    root-key]
  (binding [eval-pass shaken-map]
    (eval `(let ~(vec
                  (mapcat (clojure.core/fn [k]
                            (list (symbols k) `(~k eval-pass)))
                          shaken-keys))
             (clojure.core/fn ~(mapv (comp symbol name) input-keys)
               (let ~(vec
                      (mapcat (clojure.core/fn [k]
                                (list (symbol (name k))
                                      `(~(symbols k) ~@(map (comp symbol name)
                                                            (-> k
                                                                shaken-map
                                                                meta
                                                                :dj.compose
                                                                :dependencies)))))
                              sorted-keys))
                 ~(symbol (name root-key))))))))

;; sequential arguments
(defn debug-emitter [shaken-map
                     symbols
                     _
                     input-keys
                     sorted-keys
                     root-key]
  (let [dependencies (persistent!
                      (reduce (fn [ret k]
                                (assoc! ret
                                        k
                                        (-> k
                                            shaken-map
                                            meta
                                            :dj.compose
                                            :dependencies)))
                              (transient {})
                              sorted-keys))]
    (clojure.core/fn [& args]
      ((persistent!
        (reduce (fn [val-tree the-fn-key]
                  (let [the-fn (shaken-map the-fn-key)]
                    (assoc! val-tree
                            the-fn-key
                            (try
                              (apply the-fn (map (clojure.core/fn [k]
                                                   (val-tree k))
                                                 (dependencies the-fn-key)))
                              (catch Exception e
                                (throw (ex-info "treefn node error"
                                                (persistent! val-tree)
                                                e)))))))
                (transient (zipmap input-keys args))
                sorted-keys))
       root-key))))

;; map style arguments, reusable
(defn partial-emitter [shaken-map
                       symbols
                       _
                       _
                       sorted-keys
                       root-key]
  (let [dependencies (persistent!
                      (reduce (fn [ret k]
                                (assoc! ret
                                        k
                                        (-> k
                                            shaken-map
                                            meta
                                            :dj.compose
                                            :dependencies)))
                              (transient {})
                              sorted-keys))]
    (clojure.core/fn [partial-val-tree]
      ((persistent!
        (reduce (fn [val-tree the-fn-key]
                  (if (val-tree the-fn-key)
                    val-tree
                    (let [the-fn (shaken-map the-fn-key)]
                      (assoc! val-tree
                              the-fn-key
                              (try
                                (apply the-fn (map (clojure.core/fn [k]
                                                     (val-tree k))
                                                   (dependencies the-fn-key)))
                                (catch Exception e
                                  (throw (ex-info "treefn node error"
                                                  (persistent! val-tree)
                                                  e))))))))
                (transient partial-val-tree)
                sorted-keys))
       root-key))))

(defn treefn
  ([fn-map root-key input-keys]
     (treefn fn-map root-key input-keys inlined-emitter))
  ([fn-map root-key input-keys emitter]
     (let [input-key-set (set input-keys)
           available-keys (set (keys fn-map))
           shaken-keys ((clojure.core/fn collect [all-dependents temp-key]
                          (let [the-fn (temp-key fn-map)
                                dependents (-> the-fn
                                               meta
                                               :dj.compose
                                               :dependencies
                                               set
                                               (cs/difference input-key-set))]
                            (if (empty? dependents)
                              (conj all-dependents
                                    temp-key)
                              (reduce collect
                                      (cs/union dependents all-dependents)
                                      dependents))))
                        #{root-key}
                        root-key)
           undefined-keys (cs/difference shaken-keys available-keys)
           _ (if (empty? undefined-keys)
               nil
               (throw (Exception. (str "Unbound keys " undefined-keys))))
           shaken-map (select-keys fn-map shaken-keys)
           shaken-dag-with-inputs (reduce-kv (clojure.core/fn [ret k the-fn]
                                               (let [dependents (-> the-fn
                                                                    meta
                                                                    :dj.compose
                                                                    :dependencies)]
                                                 (assoc ret
                                                   k
                                                   (set dependents))))
                                             {}
                                             shaken-map)
           ;; remove input-keys so we know what functions to call
           shaken-dag (reduce-kv (clojure.core/fn [ret k dependents]
                                   (assoc ret
                                     k
                                     (cs/difference dependents
                                                    input-key-set)))
                                 {}
                                 shaken-dag-with-inputs)
           sorted-keys (dca/topological-sort shaken-dag)
           symbols (reduce (clojure.core/fn [ret k]
                             (assoc ret
                               k (-> k
                                     name
                                     gensym)))
                           {}
                           shaken-keys)]
       (with-meta (emitter shaken-map
                           symbols
                           shaken-keys
                           input-keys
                           sorted-keys
                           root-key)
         {:dj.compose {:dag shaken-dag-with-inputs}}))))

(defn treefm
  "shakes fm-map with root-key and produces a treefn but with fm instead of fn, which uses map destructuring

  Internally, maps are being passed around to the functions"
  ([fn-map root-key]
     (treefm fn-map root-key false))
  ([fn-map root-key return-val-map?]
     (let [available-keys (set (keys fn-map))
           ;; set of keys
           shaken-keys ((clojure.core/fn collect [all-dependents temp-key]
                          (let [the-fn (temp-key fn-map)
                                dependents (-> the-fn
                                               meta
                                               :dj.compose
                                               :dependencies
                                               set)]
                            (if (empty? dependents)
                              (conj all-dependents
                                    temp-key)
                              (reduce collect
                                      (cs/union dependents all-dependents)
                                      dependents))))
                        #{root-key}
                        root-key)
           ;; set of keys
           input-key-set (cs/difference shaken-keys available-keys)
           ;; map of keys->fms that will be called
           shaken-map (select-keys fn-map shaken-keys) ; select-keys automatically disregards keys 
           ;; map of keys->keys (also for analysis, contains topological information)
           shaken-dag-with-inputs (reduce-kv (clojure.core/fn [ret k the-fn]
                                               (let [dependents (-> the-fn
                                                                    meta
                                                                    :dj.compose
                                                                    :dependencies)]
                                                 (assoc ret
                                                   k
                                                   (set dependents))))
                                             {}
                                             shaken-map)
           ;; map of keys->keys (remove input-keys so we call only functions)
           shaken-dag (reduce-kv (clojure.core/fn [ret k dependents]
                                   (assoc ret
                                     k
                                     (cs/difference dependents
                                                    input-key-set)))
                                 {}
                                 shaken-dag-with-inputs)
           ;; vector of keys of fms we will call
           sorted-keys (dca/topological-sort shaken-dag)]
       (with-meta (clojure.core/fn [partial-val-map]
                    (let [provided-input-key-set (set (keys partial-val-map))]
                      (when-not (empty? (cs/difference input-key-set provided-input-key-set))
                        (throw (ex-info "provided input keys does not cover required inputs"
                                        {:required-input-key-set input-key-set
                                         :provided-input-key-set provided-input-key-set}))))
                    (let [ret-val-map (persistent!
                                       (reduce (fn [val-map the-fn-key]
                                                 ;; we can reuse previously computed values
                                                 (if (val-map the-fn-key)
                                                   val-map
                                                   (let [the-fn (shaken-map the-fn-key)]
                                                     (assoc! val-map
                                                             the-fn-key
                                                             (try
                                                               (the-fn val-map)
                                                               (catch Exception e
                                                                 (throw (ex-info "treefn node error"
                                                                                 (persistent! val-map)
                                                                                 e))))))))
                                               (transient partial-val-map)
                                               sorted-keys))]
                      (if return-val-map?
                        ret-val-map
                        (ret-val-map root-key))))
         {:dj.compose {:dag shaken-dag-with-inputs}}))))

#_ (do
     (let [m {:c (dj.compose.treefn/fn [n]
                   (* 3 n))
              :a (dj.compose.treefn/fn [c]
                   (inc c))
              :b (dj.compose.treefn/fn [a c d]
                   (+ a c d))}
           let-fn (dj.compose.treefn/treefn m
                                            :b
                                            [:n :d]
                                            dj.compose.treefn/debug-emitter)]
       (let-fn 0 1))

     (let [m {:c (dj.compose.treefn/fn [n]
                   (* 3 n))
              :a (dj.compose.treefn/fn [c]
                   (inc c))
              :b (dj.compose.treefn/fn [a c d]
                   (+ a c d))}
           let-fn (dj.compose.treefn/treefn m
                                            :b
                                            [:n :d]
                                            dj.compose.treefn/partial-emitter)]
       (let-fn {:n 0 :d 1 :b 10}))

     (let [m {:c (fm [n]
                     (* 3 n))
              :a (fm [c]
                     (inc c))
              :b (fm [a c d]
                     (+ a c d))}
           tfm (treefm m :b true)]
       (tfm {:n 0 :d 1}))


     (ex-data (.getCause clojure.core/*e)))
