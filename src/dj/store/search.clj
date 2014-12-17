(ns dj.store.search
  (:require [dj.store :as ds]
            [dj.paths :as dp]
            [dj.kvtransduce :as dk]))

(defn str-search-store-index
  "
splits a query-string by whitespace and requires all strings must exist in index values

filters for q in key and value

returns {:partition p :index i} -> description  
"
  [index-map ^String q]
  (if (empty? q)
    {}
    (let [word-list (.split q (str #"\s+"))]
      (persistent!
       (reduce-kv ((dk/kv-filter (fn [k v]
                                   (let [with-key-info (str (:partition k) "/" (:index k) " " v)]
                                     (every? #(re-find (re-pattern (str "(?i)"
                                                                        (java.util.regex.Pattern/quote %)))
                                                       with-key-info)
                                             word-list))))
                   assoc!)
                  (transient {})
                  index-map)))))

(defn basic-hashmap-form [k v]
  (let [sid (str (:partition k) "/" (:index k))]
    {:sid sid
     :description v}))

(defn add-folder-list [store-folder]
  (fn [k v]
    (assoc v
      :file-entries (mapv dp/file->entry (dj.io/ls (dj.io/file store-folder (:sid v)))))))

(defn sort-by-last-modified [entries]
  (sort-by (fn [{:keys [file-entries]}]
             (if (empty? file-entries)
               0
               (apply max (map :last-modified file-entries))))
           >
           entries))

(defn select [tuples keys]
  (mapv #(select-keys % keys) tuples))

(defn get-folder-list
  "transforms index-map values from description -> {:sid ... :description ... :file-list ...}"
  [index-map store-folder]
  (persistent!
   (reduce-kv ((comp
                (dk/v-map basic-hashmap-form)
                (dk/v-map (add-folder-list store-folder)))
               assoc!)
              (transient {})
              index-map)))

(defn str-search-store
  "convenience function, gets index-map and passes it to get-folder-list"
  [store-folder query-string]
  (-> (dj.io/file store-folder "db/indexes")
      ds/read-index-store-folder
      (str-search-store-index query-string)
      (get-folder-list store-folder)
      vals))
