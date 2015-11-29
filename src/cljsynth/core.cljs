(ns cljsynth.core
  (:require [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]))

(enable-console-print!)

(defn on-key [k]
  (fn [x & _]
    (k x)))

(defonce app-state
  (atom {:nodes [{:id :osc
                  :node :oscillator
                  :freq 261.6
                  :out [:gainer]}

                 {:id :gainer
                  :node :gain
                  :gain 0.5
                  :out [:dest] }]
         :built {}}))


(defmulti update-node (on-key :node))
(defmethod update-node :oscillator
  [{:keys [freq type wave] :as arg} osc]
  (when freq
    (set! (.. osc -frequency -value) freq))
  (when type
    (set! (.-type osc) type))
  (when wave
    (let [pw (.createPeriodic (.-context osc))])))

(defmethod update-node :delay
  [{:keys [delay]} del]
  (set! (.. del -delayTime -value) delay))

(defmethod update-node :gain
  [{:keys [gain]} gnode]
  (set! (.. gnode -gain -value) gain))


(defmulti make-node (on-key :node))

(defmethod make-node :oscillator
  [{:keys [freq type wave] :as node} ctx]
  (let [osc (.createOscillator ctx)]
    (update-node node osc)
    (.start osc (.-currentTime ctx))
    osc))

(defmethod make-node :delay
  [{:keys [delay]} ctx]
  (.createDelay ctx delay))

(defmethod make-node :gain
  [n ctx]
  (update-node n (.createGain ctx)))

(defmethod make-node :panner
  [_ _])

(defmethod make-node :splitter
  [{:keys [channels]
    :or {channels 2}} ctx]
  (.createChannelSplitter ctx channels))

(defmethod make-node :merger
  [{:keys [channels] :or {channels 2}} ctx]
  (.createChannelMerger ctx channels))

(defmethod make-node :script
  [{:keys [process size in out]
    :or {size 4096
         in 1
         out 1}} ctx]
  (let [node (.createScriptProcessor ctx size in out)]
    (set! (.-onaudioprocess node)
          (fn [event]
            (process (.-inputBuffer event)
                     (.-outputBuffer event))))
    node))

(defmethod make-node :convolver
  [{:keys []} ctx]
  )

(defmethod make-node :biquad
  [{:keys [detune freq q type]} ctx]
  (let [quad (.createBiquadFilter ctx)]
    (set! (.-type quad) type)
    (when q (set! (.. quad -Q -value) q))
    (when freq (set! (.. quad -frequency -value) freq))
    (when detune (set! (.. quad -detune -value) detune))))



(defn build-node-map [ctx nodes]
  (into {} (map (fn [node]
                  [(:id node)
                   (assoc node
                          ::node (make-node node ctx))]))
        nodes))

;; TODO: How to specify -destination?
(defn build-nodes [ctx nodes]
  (let [node-map (build-node-map ctx nodes)]
    ;; Connections:
    (doseq [[_ node] node-map, cxn (:out node)
            :let [from-node (::node node)]]
      (js/console.log from-node)
      (if (vector? cxn)
        (let [[to-node-name arg1 arg2] cxn
              to-node (node-map to-node-name)]
          (if (integer? arg1)
            (if (integer? arg2)
              ;; Connect from channel arg1 of from-node to channel arg2
              ;; of to-node
              (.connect from-node arg1 arg2)

              (.connect from-node arg1))

            ;; If the out value is specified as [node-name :param],
            ;; connect the from node to an audio parameter.
            (.connect from-node
                      (aget to-node (name arg1)))))


        (.connect from-node (if (= cxn :dest)
                              (.-destination ctx)
                              (node-map cxn)))))

    node-map))

(defn oscillator []
  (let [ctx (js/AudioContext.)
        osc (.createOscillator ctx)]
    (doto osc
      (.start (.-currentTime ctx))
      (.connect (.-destination ctx)))
    ctx))

(defmulti node-view (on-key :node))
(defmethod node-view :oscillator
  [{:keys [freq type wave] :as node} owner]
  (reify
    om/IRender
    (render [_]
      (html
       [:div.node.oscillator
        [:div.freq
         "Frequency:"
         [:input {:type "range"
                  :name "freq"
                  :value freq
                  :min 20
                  :max 1000
                  :onChange (fn [e]
                              (om/update! node :freq (.. e -target -value)))}]
         [:span.frequency freq "Hz"]]
        [:select {:name "type"
                  :value type
                  :onChange (fn [e]
                              (om/update! node :type (.. e -target -value)))}
         (for [t ["sine" "square" "sawtooth" "triangle" "custom"]]
           [:option {:value t
                     } t])]]))))

(defmethod node-view :gain
  [{:keys [gain] :as node} owner]
  (om/component
   (html
    [:div.node.gain
     [:div.gain
      "Gain:"
      [:input {:type "range"
               :name "gain"
               :value gain
               :min 0
               :max 10
               :onChange (fn [e]
                           (om/update! node :gain (.. e -target -value)))}]]])))

(defn system-view [app owner]
  (reify
    om/IRender
    (render [_]
      (html [:nodes
             "Nodes:"
             (for [node (:nodes app)]
               (om/build node-view
                         node
                         {:react-key (:id node)}))]))))

(defn main []
  (om/root system-view
           app-state
           {:target (.getElementById js/document "app")
            :tx-listen (fn [{:keys [path old-value new-value]} app]
                         (when (= (first path) :nodes)
                           (let [[_ n k] path]
                             (let [desc (-> app :nodes (nth n))
                                   node (get-in app [:node-map
                                                     (:id desc)
                                                     ::node])]
                               (update-node
                                (select-keys desc [k :node])
                                node)))))}))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (main)
  (when-let [ctx (:context @app-state)]
    (try
      (doto ctx
        (.suspend)
        (.close))

      (catch js/Error exc
        )))

  (swap! app-state
         (fn [app]
           (let [ctx (js/AudioContext.)
                 node-map (build-nodes ctx (:nodes app))]
             (assoc app
                    :context ctx
                    :node-map node-map))))
  #_
  (swap! app-state assoc :context (oscillator)))

(defn ^:export init []
  (on-js-reload))
