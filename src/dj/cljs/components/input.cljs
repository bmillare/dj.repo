(ns dj.cljs.input
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as cca]))

(defn input [id opts]
  (merge {:id id
          :chan:output:id nil ;; needs to be bound during connection time
          :chan:output nil
          :dom:text ""
          :dom:component (fn [data owner]
                           (let [{:keys [dom:text chan:output]} (get data id)]
                             (reify
                               om/IRender
                               (render [this]
                                 (dom/input #js {:onChange (fn [e]
                                                             (cca/go
                                                              (cca/>! chan:output
                                                                      {:id id
                                                                       :event:type :onChange
                                                                       :event:value (.. e -target -value)})))
                                                 :value dom:text})))))}
         opts))

;; on state-change
;; on key-press
;; on mouse-click

;; javascript events

;; http://www.w3schools.com/jsref/dom_obj_event.asp

;; rename to superinputdom?