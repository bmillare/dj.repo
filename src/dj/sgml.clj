(ns dj.sgml
  (:require [dj.repl]
            [clojure.string :as cs]))

;; Uses same data representation as enlive with the following additions:
;; :!tag and :?tag

;; How they will be used is still in development

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
(defn emit-tag [data]
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

(extend-type String
  IEmitForm
  (-emit-form [form]
    form))

(extend-type nil
  IEmitForm
  (-emit-form [form]
    nil))

(extend-type Object
  IEmitForm
  (-emit-form [form]
    (emit-tag form)))

(defn emit [forms]
  (apply str (map -emit-form forms)))
