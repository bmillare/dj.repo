(ns dj.store
  (:require [dj.io]
            [dj.repl]
            [dj.shell]))

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

(def index-folder "db/indexes")

;;; High-level
;; ----------------------------------------------------------------------

(defn latest [sys partition]
  (let [data (-> sys
                 :dj/store-path
                 (dj.io/file dj.store/index-folder)
                 read-index-store-folder
                 keys)
        only-p (into []
                     (comp
                      (filter (fn [k]
                                (= (:partition k)
                                   partition)))
                      (map (fn [k]
                             (Integer/parseInt (:index k)))))
                     data)
        ret (last (sort only-p))]
    ret))

(defn new-entry
  "create a new store entry with folder and stub file (opt) in partition"
  ([sys partition id & [filename]]
     (let [store-path (:dj/store-path sys)
           folder (-> store-path
                      (dj.io/file partition (str id)))
           index (dj.io/file store-path index-folder partition)]
       (if (.exists folder)
         (throw (ex-info (str (dj.io/get-path folder)
                              " entry already exists")
                         (dj.repl/local-context)))
         (do
           (when-not (zero? (dj.shell/exit-code (dj.shell/fsh "emacsclient" "-nc" index)))
             (throw (ex-info (str "failed to open index, emacsclient -nc failed")
                             (dj.repl/local-context))))
           (dj.io/mkdir folder)
           (when filename
             (let [new-file (dj.io/file folder filename)]
               (dj.io/poop new-file
                           "")
               (when-not (zero? (dj.shell/exit-code (dj.shell/fsh "emacsclient" "-nc" new-file)))
                 (throw (ex-info (str "failed to open new file, emacsclient -nc failed")
                                 (dj.repl/local-context)))))))))))
