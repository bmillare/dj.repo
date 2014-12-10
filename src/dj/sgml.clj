(ns dj.sgml
  (:require [dj.repl]
            [clojure.string :as cs]))

(declare emit)

(defprotocol IEmitForm
  (-emit-form [form]))

(defn emit-attrs [attrs]
  (cs/join " "
           (persistent!
            (reduce-kv (fn [ret k v]
                         (conj! ret
                                (str (name k) "=\"" (-emit-form v) "\"")))
                       (transient [])
                       attrs))))

;; TODO: add escaping
#_ (defn emit-tag [data]
  (if-let [tag (:tag data)]
    (let [tag:name (name tag)
          attrs:emit-pre (emit-attrs (:attrs data))
          attrs:emit (when-not (empty? attrs:emit-pre)
                       (str " " attrs:emit-pre))]
      (if-let [content (:content data)]
        (str "<" tag:name attrs:emit ">"
             (emit content)
             "</" tag:name ">")
        (str "<" tag:name attrs:emit " />")))
    (if-let [!tag (:!tag data)]
      (let [!tag:name (name !tag)]
        (if (= !tag:name "[CDATA[")
          (str "<![CDATA[" (:cdata data) "]]>")
          (let [attrs (:attrs data)
                attrs:emit (when attrs
                             (str " " attrs))
                content (:content data)
                content:emit (when content
                               (str " [" content "]"))]
            (str "<!" !tag:name attrs:emit content:emit ">"))))
      (if-let [?tag (:?tag data)]
        (str "<?" (name ?tag) (emit-attrs (:attrs data)) " ?>")
        (ex-info "No tag specified" (dj.repl/local-context))))))

(defn emit-tag [data]
  (let [tag:name (name (first data))]
    (case (first tag:name)
      \! (case tag:name
           "!--" (str "<!-- " (second data) " -->")
           ;; Does not check for ]]> in content
           "!CDATA" (str "<![CDATA[" (second data) "]]>")
           (let [content (next data)]
             (if content
               (str "<" tag:name " "
                    (cs/join " " (for [form content]
                                   (if (vector? form)
                                     (str "["(apply str (map emit-tag form)) "]")
                                     (if (string? form)
                                       (pr-str form)
                                       (name form))))) ">")
               (str "<" tag:name ">"))))
      \? (str "<" tag:name " " (emit-attrs (second data)) "?>")
      (let [[attrs & content] (rest data)]
        (let [attrs:emit-pre (emit-attrs attrs)
              attrs:emit (when-not (empty? attrs:emit-pre)
                           (str " " attrs:emit-pre))]
          (if content
            (str "<" tag:name attrs:emit ">"
                 (emit content)
                 "</" tag:name ">")
            (str "<" tag:name attrs:emit " />")))))))

(comment
  (dj.sgml/emit [[:!DOCUMENT :html
                  [[:!DOCUMENT :html :blah]
                   [:!DOCUMENT :html :blah]]]
                 [:!-- "asdf falskdfj asldfkj alskfdj "]
                 [:?asdf {:asdf "asdf"
                          :ab "bb"}]
                 [:body {:class "jasdf"} "use" "it"]
                 [:body nil nil]])
  "<!DOCUMENT html [<!DOCUMENT html blah><!DOCUMENT html blah>]><!-- asdf falskdfj asldfkj alskfdj  --><?asdf asdf=\"asdf\" ab=\"bb\"?><body class=\"jasdf\">useit</body><body></body>")

;; BUG: Strings should be escaped
(extend-type String
  IEmitForm
  (-emit-form [form]
    form))

(extend-type nil
  IEmitForm
  (-emit-form [form]
    nil))

(extend-type Number
  IEmitForm
  (-emit-form [form]
    (str form)))

(extend-type Object
  IEmitForm
  (-emit-form [form]
    (emit-tag form)))

(defn emit [forms]
  (apply str (map -emit-form forms)))

;; really it should be templates and tools that let you modify subsets
;; of the tree like first node with name 'html' etc.

;; example API
;; (update-attr [:html :head] assoc :lang "en")
;; (update-content [:html] conj [:body nil])

;; generically we need to update an element in a vector that matches
(defn update-vector
  [v match? f]
  (let [size (count v)]
       (loop [n 0]
         (if (< n size)
           (let [child (v n)]
             (if (match? child)
               (assoc v
                 n (f child))
               (recur (inc n))))
           v))))

(defn update-node [content [k & ks] f]
  (let [match? #(if (vector? %)
                  (= (first %) k)
                  nil)]
    (if ks
      (update-vector content
                     match?
                     #(update-node % ks f))
      (update-vector content
                     match?
                     f))))

(defn assoc-in-attr [content nks aks v]
  (update-node content
               nks
               #(assoc-in % (list* 1 aks)
                          v)))

(defn append-content [content ks new-content]
  (update-node content
               ks
               #(into % new-content)))

#_ (do
     (update-vector [1 2 3 4] #(= % 2)
                    #(+ % 1))
     [1 3 3 4]

     (update-node [[1 2 3]
                   [2 [4] [3]]]
                  [2 10]
                  #(conj % 0))
     [[1 2 3] [2 [4] [3]]]

     (append-content [[1 2 3]
                      [2 [4] [3]]]
                     [2 4]
                     [1 2 3 4])
     [[1 2 3] [2 [4 1 2 3 4] [3]]]
     )
