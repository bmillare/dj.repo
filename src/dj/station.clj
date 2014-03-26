(ns dj.station
  (:require [dj.io]
            [dj.paths :as dp]
            [clojure.stacktrace :as cs]))

(defn folder-stats [path]
  (try
    {:folder-sequence (dp/folder-sequence path)
     :file-entries (let [grouped (->> (mapv dp/file->entry (dj.io/ls (dj.io/file path)))
                                      (sort-by :directory?)
                                      (group-by :directory?))]
                     (vec (concat (get grouped true)
                                  (get grouped false))))}
    (catch Exception e
      {:exception (dj.io/capture-out-err
                   (cs/print-stack-trace e))})))
