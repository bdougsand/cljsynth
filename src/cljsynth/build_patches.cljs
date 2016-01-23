(ns cljsynth.build-patches)

(defn on-key [k]
  (fn [x & _]
    (k x)))




(defmulti update-node (on-key :node))
(defmethod update-node :default [_ _]
  ;; Do nothing
  )
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

(defmethod update-node :convolver
  [{:keys []} node]
  ;; do stuff?
  )


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
  (let [gain (.createGain ctx)]
    (update-node n gain)
    gain))

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
  [_ ctx]
  (.createConvolver ctx))

(defmethod make-node :biquad
  [{:keys [detune freq q type]} ctx]
  (let [quad (.createBiquadFilter ctx)]
    (set! (.-type quad) type)
    (when q (set! (.. quad -Q -value) q))
    (when freq (set! (.. quad -frequency -value) freq))
    (when detune (set! (.. quad -detune -value) detune))))
