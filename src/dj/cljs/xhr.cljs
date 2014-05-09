(ns dj.cljs.xhr)

;; http://www.w3schools.com/dom/dom_http.asp

(defn post [{:keys [response:fn url txt]}]
  (let [xhr (js/XMLHttpRequest.)]
    (doto xhr
      (.open "POST" url true)
      (aset "onload" (fn []
                       (response:fn {:status (.-status xhr)
                                     :body (.-responseText xhr)
                                     :headers (.getAllResponseHeaders xhr)})))
      (.setRequestHeader "Content-Type" "application/edn")
      (.setRequestHeader "Accept" "application/edn")
      (.send txt))))

(defn get-raw
  [{:keys [response:fn url]}]
  (let [xhr (js/XMLHttpRequest.)
        ready-state:id 4]
    (doto xhr
      (.open "GET" url true)
      (aset "onload" (fn []
                       (response:fn {:status (.-status xhr)
                                     :body (.-responseText xhr)
                                     :headers (.getAllResponseHeaders xhr)})))
      (.setRequestHeader "Content-Type" "application/edn")
      (.setRequestHeader "Accept" "application/edn")
      (.send))))

(defn get-encoded
  "instead of sending txt as body, sends it encoded in the URL"
  [args]
  (get-raw (update-in args
                      [:url]
                      (fn [url]
                        (str url "?" (.encodeURIComponent js/window (:txt args)))))))
