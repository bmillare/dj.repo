(ns dj.store.search
  (:require [dj.store :as ds]
            [dj.paths :as dp]))

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
                    :file-entries (mapv dp/file->entry (dj.io/ls (dj.io/file store-folder sid)))})))
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
