(ns dj.run
  (:require [dj]
            [dj.io]
            [clojure.java.shell :as sh]))

(defn run-vector
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
                              {:time (java.util.Date.)
                               :group-time group-time
                               :command command
                               :future (future (apply sh/sh command))})))
                      commands)]
    (swap! log
           into
           entries)))

(defn run-map
  "commands is a vector of vector args
 ex. [[\"ls\" \"-l\"]]

Executes all commands that are not already running

log form is a map, the key is the command"
  [log commands]
  (let [group-time (java.util.Date.)]
    (doseq [command commands]
      (when-not (empty? command)
        (loop []
          (let [dlog @log]
            (when (or (not (dlog command)) ; first run
                      (get-in dlog [command :finish]) ; finished
                      )
              (let [dlog' (assoc dlog
                            command
                            {:time (java.util.Date.)
                             :group-time group-time
                             :command command
                             :finish nil
                             :result nil})]
                (if (compare-and-set! log
                                      dlog
                                      dlog')
                  (let [f (future (let [ret (apply sh/sh command)]
                                    (swap! log
                                           update-in [command]
                                           assoc
                                           :finish (java.util.Date.)
                                           :result ret)
                                    ret))]
                    (swap! log
                           (fn [dlog]
                             (if (get-in dlog [command :finish])
                               dlog
                               (assoc-in dlog
                                         [command :future]
                                         f)))))
                  (recur))))))))))

(def clear-done (map (fn [entry]
                       (if-let [future-entry (:future entry)]
                         (if (future-done? future-entry)
                           (dissoc entry :future)
                           entry)
                         entry))))
;; doesn't seem to stop the process
(def cancel (map (fn [entry]
                   (if-let [future-entry (:future entry)]
                     (future-cancel future-entry)
                     entry))))
(def running (filter (fn [entry]
                       (not (future-done? (:future entry))))))
(defn has-args [& check-args]
  (filter (fn [entry]
            (every? (fn [^String check-arg]
                      (some (fn [^String command-arg]
                              (.contains command-arg
                                         check-arg))
                            (:command entry)))
                    check-args))))
