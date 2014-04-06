(ns dj.chm
  (:require [dj]
            [dj.peg :as dp]
            [dj.io]))

;; conistent human machine file format

(defn unescape [s]
  (dj/replace-map s
                  {"#:" "/"
                   "##" "#"}))

;;; todo later
;; utilities for updating files, only readers, and basic emitters
;; implemented for now

;; Problem: We can't update the hash of parents without loading all
;; the data of the parent

(let [space (dp/t #" ")
      parser (dp/alt (dp/s (dp/? (dp/s (dp/t #"#") (dp/t #"\S+") space))
                           (dp/|
                            (dp/alt (dp/t #"nil") (fn [_] {:type :nil :value nil}))
                            (dp/alt (dp/t #"true") (fn [_] {:type :bool :value true}))
                            (dp/alt (dp/t #"false") (fn [_] {:type :bool :value false}))
                            (dp/alt (dp/t #"[+-]?\d+\.\d+[M]?([eE][+-]?\d+)?")
                                    (fn [s]
                                      (let [n (read-string s)]
                                        {:type :float
                                         :value n})))
                            (dp/alt (dp/t #"[+-]?\d+N?")
                                    (fn [s]
                                      (let [n (read-string s)]
                                        {:type :integer
                                         :value n})))
                            (dp/alt (dp/s (dp/t #"char ")
                                          (dp/t #"\w"))
                                    (fn [[_ c]]
                                      {:type :char
                                       :value (first c)}))
                            (dp/alt (dp/t #":\S+")
                                    (fn [k]
                                      {:type :keyword
                                       :value (-> k
                                                  unescape
                                                  read-string)}))
                            (dp/alt (dp/s (dp/t #"'") (dp/t #"\S+"))
                                    (fn [[_ s]]
                                      {:type :symbol
                                       :value (-> s
                                                  unescape
                                                  read-string
                                                  symbol)}))
                            ;; labels are used to avoid file
                            ;; collisions They are on purpose not
                            ;; specified, if one used hashes, then you
                            ;; could also do deterministic lookups.
                            (dp/alt (dp/s (dp/t #"str") (dp/t #".*"))
                                    (fn [[_ label]]
                                      {:type :string
                                       :label label}))
                            (dp/alt (dp/s (dp/t #"list") (dp/t #".*"))
                                    (fn [[_ label]]
                                      {:type :list
                                       :label label}))
                            (dp/alt (dp/s (dp/t #"vec") (dp/t #".*"))
                                    (fn [[_ label]]
                                      {:type :vector
                                       :label label}))
                            (dp/alt (dp/s (dp/t #"map") (dp/t #".*"))
                                    (fn [[_ label]]
                                      {:type :map
                                       :label label}))
                            (dp/alt (dp/s (dp/t #"set") (dp/t #".*"))
                                    (fn [[_ label]]
                                      {:type :set
                                       :label label}))
                            (dp/alt (dp/s (dp/t #"edn") (dp/t #".*"))
                                    (fn [[_ label]]
                                      {:type :edn
                                       :label label}))))
                     (fn [[[_ tag _] type-value]]
                       (assoc type-value
                         :tag (when tag
                                (symbol tag)))))]
  (defn parse-filename [s]
    (:result (dp/parse parser s))))

(declare read-file)

(defn read-seq [^java.io.File f]
  (if (.isDirectory f)
    (let [indexes (dj.io/ls f)
          size (count indexes)]
      (for [i (range size)]
        (read-file (first (dj.io/ls (dj.io/file f (str i)))))))
    (throw (ex-info "file needs to be a directory"
                    {:file f}))))

(defn read-vec [^java.io.File f]
  (if (.isDirectory f)
    (let [indexes (dj.io/ls f)
          size (count indexes)]
      (mapv (fn [i]
              (read-file (first (dj.io/ls (dj.io/file f (str i))))))
            (range size)))
    (throw (ex-info "file needs to be a directory"
                    {:file f}))))

(defn read-set [^java.io.File f]
  (if (.isDirectory f)
    (set (map read-file (dj.io/ls f)))
    (throw (ex-info "file needs to be a directory"
                    {:file f}))))

(defn read-edn [^java.io.File f]
  (-> f
      dj.io/eat
      read-string))

(defn read-map [^java.io.File f]
  (reduce (fn [ret keyf]
            (let [o (parse-filename (dj.io/get-name keyf))
                  inline:value (:value o)
                  type (:type o)
                  keyf:extended (dj.io/file keyf "key")
                  k (case type
                      :nil inline:value
                      :bool inline:value
                      :float inline:value
                      :integer inline:value
                      :char inline:value
                      :string (dj.io/eat keyf:extended)
                      :keyword inline:value
                      :symbol inline:value
                      :vector (read-vec keyf:extended)
                      :list (read-seq keyf:extended)
                      :map (read-map keyf:extended)
                      :set (read-set keyf:extended)
                      :edn (read-edn keyf:extended))
                  v (first (for [f (dj.io/ls keyf)
                                 :when (not= "key" (dj.io/get-name f))]
                             (read-file f)))]
              (assoc ret k v)))
          {}
          (dj.io/ls f)))

(defn read-file [^java.io.File f]
  (let [o (parse-filename (dj.io/get-name f))
        {:keys [tag type value]} o
        ret (case type
             :nil value
             :bool value
             :float value
             :integer value
             :char value
             :string (dj.io/eat f)
             :keyword value
             :symbol value
             :vector (read-vec f)
             :list (read-seq f)
             :map (read-map f)
             :set (read-set f)
             :edn (read-edn f))]
    (if tag
      (let [reader-var (*data-readers* tag)]
        (if reader-var
          (reader-var
           ret)
          (if *default-data-reader-fn*
            (*default-data-reader-fn* tag ret)
            (throw (ex-info "missing data reader"
                            {:readers *data-readers*
                             :tag tag
                             :type type
                             :value value})))))
      ret)))

(defn escape [txt]
  (dj/replace-map txt
                  {"/" "#:"
                   "#" "##"}))

(defprotocol Emit
  (-emit [this folder labeler])
  ;; emit-key must return the folder created
  (-emit-key [this folder labeler]))

(extend nil
  Emit
  {:-emit (fn [this folder labeler]
            (doto (java.io.File. folder "nil")
              (.createNewFile)))
   :-emit-key (fn [this folder labeler]
                (doto (dj.io/file folder "nil")
                  (dj.io/mkdir)))})

(defn emit-inline-file [this ^java.io.File folder labeler]
  (doto (java.io.File. folder (pr-str this))
    (.createNewFile)))

(defn emit-inline-folder [this folder labeler]
  (doto (dj.io/file folder (pr-str this))
    (dj.io/mkdir)))

(extend java.lang.Boolean
  Emit
  {:-emit emit-inline-file
   :-emit-key emit-inline-folder})

(extend-type java.lang.Character
  Emit
  (-emit [this folder labeler]
    (doto (java.io.File. folder (str "char " (pr-str this)))
      (.createNewFile)))
  (-emit-key [this folder labeler]
    (doto (dj.io/file folder (str "char" (pr-str this)))
      (dj.io/mkdir))))

(extend java.lang.Number
  Emit
  {:-emit emit-inline-file
   :-emit-key emit-inline-folder})

(extend-type java.lang.String
  Emit
  (-emit [this folder labeler]
    (dj.io/poop (dj.io/file folder (str "str" (labeler this)))
                this))
  (-emit-key [this folder labeler]
    (let [str-folder (dj.io/file folder (str "str" (labeler this)))]
      (dj.io/mkdir str-folder)
      (dj.io/poop (dj.io/file str-folder "key")
                  this)
      str-folder)))

(extend-type clojure.lang.Keyword
  Emit
  (-emit [this folder labeler]
    (-> (dj.io/file folder
                    (-> this
                        pr-str
                        escape))
        (.createNewFile)))
  (-emit-key [this folder labeler]
    (doto (dj.io/file folder
                      (-> this
                          pr-str
                          escape))
      (dj.io/mkdir))))

(extend-type clojure.lang.Symbol
  Emit
  (-emit [this folder labeler]
    (-> (dj.io/file folder
                    (str "'"
                         (-> this
                             pr-str
                             escape)))
        (.createNewFile)))
  (-emit-key [this folder labeler]
    (doto (dj.io/file folder
                      (str "'"
                           (-> this
                               pr-str
                               escape)))
      (dj.io/mkdir))))

(defn no-label [this]
  nil)

(extend-type java.lang.Object
  Emit
  (-emit [this folder labeler]
    (let [emit-seq (fn [type-name]
                     (let [seq-count (count this)
                           seq-folder (dj.io/file folder (str type-name (labeler this)))]
                       (dj.io/mkdir seq-folder)
                       (doall
                        (map-indexed (fn [i x]
                                       (let [indexed-folder (dj.io/file seq-folder (str i))]
                                         (dj.io/mkdir indexed-folder)
                                         (-emit x indexed-folder no-label)))
                                     this))))]
      (cond
       (map? this) (let [map-folder (dj.io/file folder (str "map" (labeler this)))]
                     (doall (map (fn [[k v]]
                                   (let [key-folder (-emit-key k map-folder hash)]
                                     (-emit v key-folder no-label)))
                                 this)))
       (vector? this) (emit-seq "vec")
       (set? this) (let [set-folder (dj.io/file folder (str "set" (labeler this)))]
                     (dj.io/mkdir set-folder)
                     (doall (map #(-emit % set-folder labeler)
                                 this)))
       (list? this) (emit-seq "list"))))
  (-emit-key [this folder labeler]
    (let [emit-seq (fn [type-name]
                     (let [seq-count (count this)
                           key-folder (dj.io/file folder (str type-name (labeler this)))
                           seq-folder (dj.io/file key-folder "key")]
                       (dj.io/mkdir seq-folder)
                       (doall
                        (map-indexed (fn [i x]
                                       (let [indexed-folder (dj.io/file seq-folder (str i))]
                                         (dj.io/mkdir indexed-folder)
                                         (-emit x indexed-folder no-label)))
                                     this))))]
      (cond
       (map? this) (let [key-folder (dj.io/file folder (str "map" (labeler this)))
                         map-folder (dj.io/file key-folder "key")]
                     (doall (map (fn [[k v]]
                                   (let [key-folder' (-emit-key k map-folder hash)]
                                     (-emit v key-folder' no-label)))
                                 this)))
       (vector? this) (emit-seq "vec")
       (set? this) (let [key-folder (dj.io/file folder (str "set" (labeler this)))
                         set-folder (dj.io/file key-folder "key")]
                     (dj.io/mkdir set-folder)
                     (doall (map #(-emit % set-folder hash)
                                 this)))
       (list? this) (emit-seq "list")))))

(defn emit-file
  "use only when starting from scratch"
  [data ^java.io.File folder]
  (-emit data folder no-label))
