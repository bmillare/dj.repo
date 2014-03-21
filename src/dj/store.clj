(ns dj.store)

(defn read-index-store*
  "
read a single file-index into an in-memory index
"
  [^java.io.File file]
  (persistent!
   (reduce (fn [m ^String s]
             (let [[idx body] (.split s
                                      (str #" ")
                                      2)
                   partition (.getName file)]
               (assoc! m
                       {:partition partition
                        :index idx}
                       body)))
           (transient {})
           (-> (slurp file)
               (.split (str #"\r\n|[\r\n]"))))))

(let [memoized (memoize (fn [file ts]
                          (read-index-store* file)))]
  (defn read-index-store
    "
read a single file-index into an in-memory index, do so only if file changed
"
    [^java.io.File file]
    (memoized file (.lastModified file))))

(defn read-index-store-folder
  "
read all file indexes and merge into single in-memory index
"
  [^java.io.File folder]
  (apply merge (map read-index-store (.listFiles folder))))

