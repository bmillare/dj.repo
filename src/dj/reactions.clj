(ns dj.reactions)

;; compile a function that executes actions when certain rules are true

;; binding-check check values at start of root node, actions thread the state through

(defn ->actions [actions]
  (fn [data e]
    (loop [as actions
           d data]
      (if (empty? as)
        d
        (let [a (first as)]
          (recur (next as)
                 (a d e)))))))

(defn ->node [binding-check action-fn children-fn else-fn]
  (if children-fn
    (if else-fn
      ;; template fn, others are for degenerate cases
      (fn [data0 datai e]
        (if (binding-check data0 e)
          (let [dataii (actions-fn datai e)]
            (children-fn data0 dataii e))
          (else-fn data0 datai e)))
      (fn [data0 datai e]
        (if (binding-check data0 e)
          (let [dataii (actions-fn datai e)]
            (children-fn data0 dataii e)))))
    (if else-fn
      (fn [data0 datai e]
        (if (binding-check data0 e)
          (actions-fn datai e)
          (else-fn data0 datai e)))
      (fn [data0 datai e]
        (if (binding-check data0 e)
          (actions-fn datai e))))))

;; inlining for performance
(defn ->event-check
  ([v k1]
     (fn [data0 e]
       (= (-> e
              (get k1))
          v)))
  ([v k1 k2]
     (fn [data0 e]
       (= (-> e
              (get k1)
              (get k2))
          v)))
  ([v k1 k2 k3]
     (fn [data0 e]
       (= (-> e
              (get k1)
              (get k2)
              (get k3))
          v)))
  ([v k1 k2 k3 k4]
     (fn [data0 e]
       (= (-> e
              (get k1)
              (get k2)
              (get k3)
              (get k4))
          v))))

;; inlining for performance
(defn ->data-check
  ([v k1]
     (fn [data0 e]
       (= (-> data0
              (get k1))
          v)))
  ([v k1 k2]
     (fn [data0 e]
       (= (-> data0
              (get k1)
              (get k2))
          v)))
  ([v k1 k2 k3]
     (fn [data0 e]
       (= (-> data0
              (get k1)
              (get k2)
              (get k3))
          v)))
  ([v k1 k2 k3 k4]
     (fn [data0 e]
       (= (-> data0
              (get k1)
              (get k2)
              (get k3)
              (get k4))
          v))))



;; :e for event
;; :data for data
;; [:e :key value]

(defn ->children* [children bindings]
  (let [binding0 (first bindings)
        rbindings (next bindings)
        child (get children binding0)
        children:child (:children child)
        [t & ks-v] binding0
        ks (drop-last ks-v)
        v (last ks-v)]
    (->node (if (= t :e)
              (apply ->event-check v ks)
              (if (= t :data)
                (apply ->data-check v ks)
                :error))
            (->actions (:actions child))
            (if (empty? children:child)
              nil
              (->children* children:child
                           (keys children:child)))
            (if (empty? rbindings)
              nil
              (->children* children
                           rbindings)))))

(defn ->children [children]
  (let [bindings (keys children)]
    (->children* children bindings)))