(ns dj.paths
  (:require [dj.io]))

;; Requires java7 or higher

(defn file->entry [^java.io.File file]
  {:name (dj.io/get-name file)
   :path (dj.io/get-path file)
   :directory? (.isDirectory file)
   :last-modified (.lastModified file)
   :size (let [url (.. file toURI toURL)]
           (with-open [stream (.openStream url)]
             (.available stream)))
   :mime (-> file
             (.toPath)
             (java.nio.file.Files/probeContentType))})

(defn folder-sequence [path]
  (let [normalized-path (-> path
                            dj.io/file
                            (.toPath)
                            (.normalize))
        path-sequence (loop [np normalized-path
                             ret []]
                        (if (= np (.getRoot normalized-path))
                          (-> (conj ret np)
                              reverse)
                          (recur (.getParent np)
                                 (conj ret np))))]
    (mapv (fn [^java.nio.file.Path path]
            {:name (.toString (.getName path (int (dec (.getNameCount path)))))
             :path (.toString path)})
          (drop 1 path-sequence))))
