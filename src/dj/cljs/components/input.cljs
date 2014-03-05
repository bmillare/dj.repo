(ns dj.cljs.inputdom
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defn inputdom [id opts]
  (merge {:id id
          :chan:output:id nil ;; needs to be set on creation
          :dom:component (fn [data owner]
                           (reify
                             om/IRender
                             (render [this]
                               (dom/input #js {:onChange (fn [e]
                                                           (om/set-state! owner
                                                                          :text
                                                                          (.. e -target -value)))
                                               :value (:text state)}))))}
         opts))

;; on state-change
;; on key-press
;; on mouse-click

;; javascript events

;; http://www.w3schools.com/jsref/dom_obj_event.asp

;; rename to superinputdom?