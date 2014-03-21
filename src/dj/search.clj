(ns dj.search
  (:require [dj.store :as ds]))

;; switch to new user interface, first search

(defn str-search-store-index
  "
splits a query-string by whitespace and requires all strings must exist in index values

filters for q in key and value
"
  [index-map ^String q]
  (if (empty? q)
    {}
    (let [word-list (.split q (str #"\s+"))]
      (persistent!
       (reduce-kv (fn [m k v]
                    (let [with-key-info (str (:partition k) "/" (:index k) " " v)]
                      (if (every? #(re-find (re-pattern (str "(?i)"
                                                             (java.util.regex.Pattern/quote %)))
                                            with-key-info)
                                  word-list)
                        (assoc! m
                                k
                                v)
                        m)))
                  (transient {})
                  index-map)))))

(defn add-folder-list [index-map store-folder]
  (reduce-kv (fn [ret k v]
               (let [sid (str (:partition k) "/" (:index k))]
                 (assoc ret
                   sid
                   {:description v
                    :file-paths (mapv dj.io/get-name (dj.io/ls (dj.io/file store-folder sid)))})))
             {}
             index-map))

(defn str-search-store
  "
Returns a function the browser will call with the search query-string

index-folder: folder with all the indexes
display-fn: given a result returns result-dom
cont: a fn that updates the page from the server
"
  [store-folder query-string]
  (-> (dj.io/file store-folder "db/indexes")
      ds/read-index-store-folder
      (str-search-store-index query-string)
      (add-folder-list store-folder)))

#_ (defn find-files [root-dir exclusions]
  (let [ff (fn find-files* [dir ret]
             (reduce (fn [ret ^java.io.File file]
                       (let [path (dj.io/get-path file)]
                         (if (exclusions path)
                           ret
                           (if (.isDirectory file)
                             (assoc! (find-files* file ret)
                                     path
                                     true)
                             (assoc! ret
                                     path
                                     nil)))))
                     ret
                     (dj.io/ls dir)))]
    (persistent!
     (ff root-dir (transient {})))))

#_ (defn query-find
  "
splits a query-string by whitespace and requires all strings must exist in index values

search in key
"
  [index-map ^String q]
  (if (empty? q)
    {}
    (let [word-list (.split q (str #"\s+"))]
      (persistent!
       (reduce-kv (fn [m k v]
                    (if (every? #(re-find (re-pattern (str "(?i)"
                                                           (java.util.regex.Pattern/quote %)))
                                          k)
                                word-list)
                      (assoc! m
                              k
                              v)
                      m))
                  (transient {})
                  index-map)))))

#_ (defn regex-search-vals
  "
splits a query-string by whitespace and requires all strings must exist in index values

search in values
"
  [index-map ^String q]
  (if (empty? q)
    {}
    (let [word-list (.split q (str #"\s+"))]
      (persistent!
       (reduce-kv (fn [m k v]
                    (if (every? #(re-find (re-pattern (str "(?i)"
                                                           (java.util.regex.Pattern/quote %)))
                                          v)
                                word-list)
                      (assoc! m
                              k
                              v)
                      m))
                  (transient {})
                  index-map)))))

#_ (defn filename-search-call-fn [root-dir ignore-fn cont emit-filebrowser emit-editor]
  (let [display-fn (file-result-fn emit-filebrowser
                                   emit-editor)
        limit-results (fn [m]
                        (select-keys m
                                     (take 25 (keys m))))
        update-files (fn []
                       (dj.view.search/find-files root-dir
                                                  ignore-fn))
        files (atom (update-files))
        last-call (atom (System/nanoTime))]
    (fn [query-string]
      (when (> (- (System/nanoTime)
                  @last-call)
               (* 1e9 10))
        (reset! last-call (System/nanoTime))
        (reset! files (update-files)))
      (-> @files
          (query-find query-string)
          limit-results
          (results-dom display-fn)
          cont))))
