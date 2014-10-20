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

(let [t (type #"")]
  (defn f*
    "returns ls filtered of path with last arg as filter regexp"
    [& args]
    (let [arg-count (count args)
          l (last args)]
      (if (= (type l)
             t)
        (case arg-count
          0 []
          1 (into []
                  (filter (fn [f]
                            (re-find l
                                     (dj.io/get-name f))))
                  (-> (dj.io/file "/")
                      dj.io/ls))
          (into []
                (filter (fn [f]
                          (re-find l
                                   (dj.io/get-name f))))
                (-> (apply dj.io/file (drop-last args))
                    dj.io/ls)))
        (throw (ex-info "last arg is not a regexp"
                        (dj.repl/local-context))))))
  (defn fr
    "returns 'find' filtered of path with last arg as filter regexp"
    [& args]
    (let [arg-count (count args)
          l (last args)
          find-files (fn [root-dir]
                       (let [ff (fn find-files* [dir ret]
                                  (reduce (fn [ret ^java.io.File file]
                                            (let [fname (dj.io/get-name file)]
                                              (if (.isDirectory file)
                                                (if (re-find l fname)
                                                  (let [prev-count (count ret)
                                                        results (find-files* file ret)]
                                                    (if (= prev-count
                                                           (count results))
                                                      (conj! results
                                                             file)
                                                      results))
                                                  (find-files* file ret))
                                                (if (re-find l fname)
                                                  (conj! ret
                                                         file)
                                                  ret))))
                                          ret
                                          (dj.io/ls dir)))]
                         (persistent!
                          (ff root-dir (transient [])))))]
      (if (= (type l)
             t)
        (case arg-count
          0 []
          1 (find-files (dj.io/file "/"))
          (find-files (apply dj.io/file (drop-last args))))
        (throw (ex-info "last arg is not a regexp"
                        (dj.repl/local-context)))))))

(defn f
  "takes first element of f*"
  [& args]
  (first (apply f* args)))

