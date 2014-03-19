(ns dj.cljs.components
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.set :as cs]
            [cljs.core.async :as cca])
  (:require-macros [cljs.core.async.macros :as ccam]))

(def keyCodes {:enter 13})

(defn focus-owner [owner]
  (.focus (om/get-node owner)))

(defn cursor-end [owner]
  (let [dom (om/get-node owner)
        len (* (.-length (.-value dom)) 2)]
    (.setSelectionRange dom len len)))

(defn event-external-refocus
  "requires that external has chan:external and target has redirect-focus:id"
  [id data owner_]
  (let [{:keys [redirect-focus:id]} (get data id)
        {:keys [chan:external]} (get data redirect-focus:id)]
    (fn [e_]
      (cca/put! chan:external
                focus-owner))))

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
        (cca/put! chan:output
                  {:sender:id id
                   :event:type :onChange
                   :event:value txt})))))

(defn onChange-push
  "pushes new text onto chan:output"
  [id data owner_]
  (let [{:keys [chan:output]} (get data id)]
    (fn [e]
      (.preventDefault e)
      ;; need to extract value right away before go block
      (let [target (.-target e)
            txt (.-value target)]
        (cca/put! chan:output
                  {:sender:id id
                   :event:type :onChange
                   :event:value txt})))))

(defn onKeyUp-push
  [id data owner_]
  (let [{:keys [chan:output]} (get data id)]
    (fn [e]
      ;; need to extract value right away before go block
      (let [keyCode (.-keyCode e)]
        (cca/put! chan:output
                  {:sender:id id
                   :event:type :onKeyUp
                   :event:value keyCode})))))

(defn rxn:update-:dom:text [id opts]
  (merge (let [component:id (:component:id opts)]
           {:id id
            :rules [[:e :event:type :onChange]
                    [:e :sender:id component:id]]
            :action (fn [data e]
                      (assoc-in data [component:id :dom:text] (:event:value e)))})
         opts))

(defn input [id opts]
  (merge {:id id
          :reactions:owned-ids #{} ;; auto-deactivating of owned reactions be a reaction or a deactivate hook, or maybe part of a generic destructor hook?
          :chan:output:id nil ;; needs to be bound during connection time
          :chan:output nil
          :chan:external:id nil ;; receive channel of functions, will pass owner to function, userful for refocusing
          :chan:external nil
          :event:onChange nil
          :event:onKeyUp nil
          :dom:text ""
          :dom:style nil
          :dom:component (fn [data owner]
                           (.log js/console "reify")
                           (let [{:keys [event:onKeyUp event:onChange dom:text chan:external dom:style chan:output]} (get data id)
                                 onChange-fn (when event:onChange
                                               (event:onChange id data owner))
                                 onKeyUp-fn (when event:onKeyUp
                                              (event:onKeyUp id data owner))]
                             ;; I am guessing we only need reify for beyond just IRender cases
                             (reify
                               ;; BUG? om seems to call this twice instead of once
                               om/IWillMount
                               (will-mount [this]
                                 ;; BUG: this channel is permanently bound, no point in dynamic behavior
                                 (.log js/console "willmount")
                                 #_ (ccam/go
                                  (loop []
                                    (when-let [f (cca/<! chan:external)]
                                      (f owner)
                                      (recur)))))
                               om/IDidMount
                               (did-mount [this]
                                 (.log js/console "didmount")
                                 ;; work around unfocus bug
                                 #_ (focus-owner owner)
                                 #_ (cursor-end owner))
                               om/IWillUpdate
                               (will-update [this _ _]
                                 (.log js/console "willupdate"))
                               om/IRender
                               (render [this]
                                 (.log js/console "render")
                                 (dom/input #js {:onChange onChange-fn
                                                 :onKeyUp onKeyUp-fn
                                                 ;; lose focus bug not due to onBlur, but due to no reestablishing focus with om
                                                 :value dom:text
                                                 :style dom:style})))))}
         opts))
