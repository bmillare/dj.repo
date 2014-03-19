(ns dj.cljs.xhr)

;; http://www.w3schools.com/dom/dom_http.asp

(defn post [{:keys [response:fn url txt]}]
  (let [xhr (js/XMLHttpRequest.)
        ready-state:id 4]
    (doto xhr
      (.open "POST" url true)
      (aset "onreadystatechange" (fn []
                                   (if (= (.-readyState xhr) ready-state:id)
                                     (response:fn {:status (.-status xhr)
                                                   :body (.-responseText xhr)
                                                   :headers (.getAllResponseHeaders xhr)}))))
      (.setRequestHeader "Content-Type" "application/edn")
      (.setRequestHeader "Accept" "application/edn")
      (.send txt))))
