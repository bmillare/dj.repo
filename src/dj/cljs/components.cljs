(ns dj.cljs.components
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.set :as cs]
            [cljs.core.async :as cca])
  (:require-macros [cljs.core.async.macros :as ccam]))

;; BUG: This overwrites existing reactions
;; Need to keep track of:
;; -id of reaction
;; no such things as parents, only users

;; a reaction is just like any other entity, we check for duplicates by checking the reaction id
(defn install-reactions [data parent:id reactions]
  (let [{:keys [reactions-active:index]} data]
    (-> data
        (update-in [parent:id :reaction:ids]
                   (fn [reaction:ids]
                     (if reaction:ids
                       (cs/union reaction:ids .))))
        (assoc (merge data rule-index)
          :reactions:active (vec (vals rule-index))))))

(def keyCodes {:enter 13})

(defn focus-owner [owner]
  (.focus (om/get-node owner)))

(defn onChange-ignore-and-push
  "pushes new text onto chan:output but then reverts it back to previous form"
  [id data owner_]
  (let [{:keys [dom:text chan:output]} (get data id)]
    (fn [e]
      (.preventDefault e)
      ;; need to extract value right away before go block
      (let [txt (.-value (.-target e))]
        ;; undo change in textbox
        (set! (.-value (.-target e))
              dom:text)
        (ccam/go
         (cca/>! chan:output
                 {:id id
                  :event:type :onChange
                  :event:value txt}))))))

(defn onChange-push
  "pushes new text onto chan:output"
  [id data owner_]
  (let [{:keys [dom:text chan:output]} (get data id)]
    (fn [e]
      (.preventDefault e)
      ;; need to extract value right away before go block
      (let [txt (.-value (.-target e))]
        (ccam/go
         (cca/>! chan:output
                 {:id id
                  :event:type :onChange
                  :event:value txt}))))))

(defn onKeyUp-push
  [id data owner_]
  (let [{:keys [chan:output]} (get data id)]
    (fn [e]
      ;; need to extract value right away before go block
      (let [keyCode (.-keyCode e)]
        (ccam/go
         (cca/>! chan:output
                 {:id id
                  :event:type :onKeyUp
                  :event:value keyCode}))))))

(defn event-external-refocus
  "requires that external has chan:external and target has redirect-focus:id"
  [id data owner_]
  (let [{:keys [redirect-focus:id]} (get data id)
        {:keys [chan:external]} (get data redirect-focus:id)]
    (fn [e_]
      (ccam/go
       (cca/>! chan:external
               focus-owner)))))

(defn input [id opts]
  (merge {:id id
          :reactions [(fn [data e]
                        (if (= (:event:type e) :onChange)
                          (assoc-in data [id :dom:text] (:event:value e))
                          data))]
          :reaction:ids #{}
          :chan:output:id nil ;; needs to be bound during connection time
          :chan:output nil
          :chan:external:id nil ;; receive channel of functions, will pass owner to function, userful for refocusing
          :chan:external nil
          :event:onChange nil
          :event:onKeyUp nil
          :dom:text ""
          :dom:style nil
          :dom:component (fn [data owner]
                           (let [{:keys [event:onKeyUp event:onChange dom:text chan:external dom:style]} (get data id)
                                 onChange-fn (when event:onChange
                                               (event:onChange id data owner))
                                 onKeyUp-fn (when event:onKeyUp
                                              (event:onKeyUp id data owner))]
                             ;; I am guessing we only need reify for beyond just IRender cases
                             (reify
                               om/IWillMount
                               (will-mount [this]
                                 (ccam/go
                                  (loop []
                                    (when-let [f (cca/<! chan:external)]
                                      (f owner))
                                    (recur))))
                               om/IRender
                               (render [this]
                                 (dom/input #js {:onChange onChange-fn
                                                 :onKeyUp onKeyUp-fn
                                                 :value dom:text
                                                 :style dom:style})))))}
         opts))
