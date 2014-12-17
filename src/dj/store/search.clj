(ns dj.store.search
  (:require [dj.store :as ds]
            [dj.paths :as dp]))

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

;; attempt at making transducer like functions for hash-maps
(defn kv-comp [rf & fs]
  (let [f ((apply comp fs) list)]
    (fn [m k v]
      (let [[k' v'] (f k v)]
        (rf m
            k' v')))))

(defn basic-hashmap-form [cont]
  (fn [k v]
    (cont k
          (let [sid (str (:partition k) "/" (:index k))]
            {:sid sid
             :description v}))))

(defn add-folder-list [store-folder]
  (fn [cont]
    (fn [k v]
      (cont k
            (assoc v
              :file-entries (mapv dp/file->entry (dj.io/ls (dj.io/file store-folder (:sid v)))))))))

(defn get-folder-list
  "transforms index-map values from description -> {:sid ... :description ... :file-list ...}"
  [index-map store-folder]
  (persistent!
   (reduce-kv (kv-comp assoc!
                       basic-hashmap-form
                       (add-folder-list store-folder))
              (transient {})
              index-map)))

(defn str-search-store
  "convenience function, gets index-map and passes it to get-folder-list"
  [store-folder query-string]
  (-> (dj.io/file store-folder "db/indexes")
      ds/read-index-store-folder
      (str-search-store-index query-string)
      (get-folder-list store-folder)))
