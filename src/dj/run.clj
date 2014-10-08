(ns dj.run
  (:require [dj]
            [dj.io]
            [dj.shell :as ds]))

#_ (defn run-vector
  "commands is a vector of vector args
 ex. [[\"ls\" \"-l\"]]

Executes all commands

log form is a vector"
  [log commands]
  (let [group-time (java.util.Date.)
        entries (into []
                      (comp
                       (filter (complement empty?))
                       (map (fn [command]
                              (merge
                               (apply ds/proc command)
                               {:time (java.util.Date.)
                                :group-time group-time
                                :command command}))))
                      commands)]
    (swap! log
           into
           entries)))

(defn init-run-map []
  {:id-gen (java.util.concurrent.atomic.AtomicInteger.)
   :data {}})

(defn nextid [^java.util.concurrent.atomic.AtomicInteger x]
  (.getAndIncrement x))

(defn run-map
  "commands is a vector of vector args
 ex. [[\"ls\" \"-l\"]]

Executes all commands given condition

  condition? accepts command and dlog
  tx accepts the id/addition and the new db state, it must return new state
  on-success accepts id and proc-map
"
  [{:keys [log condition? tx on-success commands]}]
  (let [group-time (java.util.Date.)]
    (doseq [command commands]
      (when-not (empty? command)
        (loop []
          (let [dlog @log
                id-gen (:id-gen dlog)]
            (when (condition? command dlog)
              ;; If we replace this with datomic, we need a way to duplicate 'if we succeeded'
              ;; You can either use datomic's :db.fn/cas or add an owner id and check if the id is you
              (let [id (nextid id-gen)
                    entry {:id id
                           :time (java.util.Date.)
                           :group-time group-time
                           :command command}]
                (if (compare-and-set! log
                                      dlog
                                      (tx id
                                          entry
                                          (assoc-in dlog
                                                    [:data id]
                                                    entry)))
                  (let [proc (apply ds/proc command)]
                    (swap! log
                           update-in
                           [:data id]
                           merge
                           proc)
                    (on-success id proc))
                  (recur))))))))))

(defn stringify [log]
  (fn [id proc]
    (let [out (future (ds/stream-to-string proc :out))
          err (future (ds/stream-to-string proc :err))
          exit-code (future (ds/exit-code proc))]
      (future (swap! log
                     update-in
                     [:data id]
                     assoc
                     :out-str @out
                     :err-str @err
                     :exit-code @exit-code
                     :finished (java.util.Date.))))))

(def destroy (map (fn [entry]
                    (if (:process entry)
                      (do (ds/destroy entry)
                          entry)
                      entry))))
(def running? (filter (fn [entry]
                       (not (:finished entry)))))
(defn has-args [& check-args]
  (filter (fn [entry]
            (every? (fn [^String check-arg]
                      (some (fn [^String command-arg]
                              (.contains command-arg
                                         check-arg))
                            (:command entry)))
                    check-args))))
