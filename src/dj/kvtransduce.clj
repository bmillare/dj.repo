(ns dj.kvtransduce)

(defn k-map [f]
  (fn [rf]
    (fn [m k v]
      (rf m (f k v) v))))

(defn v-map [f]
  (fn [rf]
    (fn [m k v]
      (rf m k (f k v)))))

(defn kv-map [fk fv]
  (fn [rf]
    (fn [m k v]
      (rf m (fk k v) (fv k v)))))

(defn k-filter [pred]
  (fn [rf]
    (fn [m k v]
      (if (pred k)
        (rf m k v)
        m))))

(defn v-filter [pred]
  (fn [rf]
    (fn [m k v]
      (if (pred v)
        (rf m k v)
        m))))

(defn kv-filter [pred]
  (fn [rf]
    (fn [m k v]
      (if (pred k v)
        (rf m k v)
        m))))

(defn k-remove [pred]
  (fn [rf]
    (fn [m k v]
      (if (pred k)
        m
        (rf m k v)))))

(defn v-remove [pred]
  (fn [rf]
    (fn [m k v]
      (if (pred v)
        m
        (rf m k v)))))

(defn kv-remove [pred]
  (fn [rf]
    (fn [m k v]
      (if (pred k v)
        m
        (rf m k v)))))
