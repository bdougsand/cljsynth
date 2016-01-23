(ns cljsynth.core
  (:require [goog.dom :as gdom]
            [om.core :as om]
            [clojure.string :as str]
            [sablono.core :as html :refer-macros [html]]

            [cljsynth.build-patches :refer [update-node make-node]]
            [cljsynth.state :as state]
            [cljsynth.visualize :as v]))

(enable-console-print!)

(defonce app-state
  (atom {
         ;; Pure data representation of the nodes in the system and their
         ;; relationships.
         :nodes [{:id :osc
                  :node :oscillator
                  :freq 261.6
                  :out [:gainer]}

                 {:id :gainer
                  :node :gain
                  :gain 0.5
                  :out [:dest] }]
         ;; Stores the constructed nodes
         :node-map {}}))

(def node-ids
  (memoize (fn node-ids [nodes]
             (into #{} (map :id) nodes))))

(defn build-node-map [ctx nodes]
  (into {} (map (fn [node]
                  [(:id node)
                   (assoc node
                          ::node (make-node node ctx))]))
        nodes))

(defn add-analyzer [ctx node-map]
  (assoc node-map
         ::analyzer (let [node (doto (.createAnalyser ctx)
                                 (.connect (.-destination ctx)))]
                      {::node node
                       :render (v/make-render-fn node)})))

(defn build-nodes [ctx nodes]
  (let [node-map (add-analyzer ctx (build-node-map ctx nodes))]
    ;; Connections:
    (doseq [[_ node] node-map, cxn (:out node)
            :let [from-node (::node node)]]
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
                              (-> node-map ::analyzer ::node)
                              (::node (node-map cxn))))))

    node-map))

(defn node-selector [nodes cb]
  (html
   [:select
    (for [node nodes
          :let [id (name (:id nodes))]]
      [:option {:value id} id])]))


(defn on-key [k]
  (fn [x & _] (k x)))

(defmulti node-view (on-key :node))
(defmethod node-view :oscillator
  [{:keys [freq type wave] :as node} owner]
  (reify
    om/IRender
    (render [_]
      (html
       [:div.node.oscillator
        [:div.freq
         "Frequency: "
         [:input {:type "range"
                  :name "freq"
                  :value freq
                  :min 20
                  :max 1000
                  :onChange (fn [e]
                              (om/update! node :freq (.. e -target -value)))}]
         [:span.frequency freq "Hz"]]
        [:div.shape
         "Shape: "
         [:select {:name "type"
                   :value type
                   :onChange (fn [e]
                               (om/update! node :type (.. e -target -value)))}
          (for [t ["sine" "square" "sawtooth" "triangle" "custom"]]
            [:option {:value t
                      } t])]]]))))

(defmethod node-view :gain
  [{:keys [gain] :as node} owner]
  (om/component
   (html
    [:div.node.gain
     [:div.gain
      "Gain: "
      [:input {:type "range"
               :name "gain"
               :value gain
               :min 0
               :max 10
               :step 0.1
               :onChange (fn [e]
                           (om/update! node :gain (.. e -target -value)))}]
      [:span.gain gain "×"]]])))

(defn visualizer-view [analyzer owner]
  (reify
    om/IDidMount
    (did-mount [_]
      (let [node (om/get-node owner)
            {:keys [render]} analyzer]
        (when render
          (js/requestAnimationFrame #(render node)))))

    om/IRender
    (render [_]
      (html
       [:canvas {:width 300 :height 200}]))))

(defn system-view [app owner]
  (reify
    om/IRender
    (render [_]
      (html
       [:app
        [:nodes
         (for [node (:nodes app)]
           [:div.node-config
            [:div.node-head
             [:div.node-name (name (:id node))]]
            (om/build node-view
                      node
                      {:react-key (:id node)})
            (when-let [out (:out node)]
              (str "Out: " (str/join "," out)))])]
        [:div.visualizer
         (om/build visualizer-view (-> app :node-map ::analyzer))]
        [:form.system-controls
         {:onSubmit (fn [e]
                      (om/transact! app state/add-new :oscillator))}
         [:select {:name "node_type"}
          (for [nt [:oscillator :convolver :gain :delay]]
            [:option {:value (name nt)}
             (str/capitalize (name nt))])]
         [:button "Add"]]]))))

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
    (doto ctx
      (.suspend)
      (.close)))
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
  (main))
