(ns cljsynth.visualize
  (:require [goog.style :as style]))

(defn make-render-fn [analyzer]
  (set! (.-fftSize analyzer) 2048)
  (let [blen (.-frequencyBinCount analyzer)
        buffer (js/Uint8Array. blen)]
    (fn draw [canvas]
      (let [ctx (.getContext canvas "2d")
            size (style/getSize canvas)
            w (.-width size)
            h (.-height size)
            h2 (/ h 2)]
        (.getByteTimeDomainData analyzer buffer)

        (set! (.-fillStyle ctx) "rgb(200,0,200)")
        (set! (.-lineWidth ctx) 2)

        (doto ctx
          (.clearRect 0 0 w h)
          (.beginPath)
          (.moveTo 0 0))
        (let [dx (/ w blen)]
          (dotimes [i blen]
            (.lineTo ctx
                     (* i dx)
                     (* (/ (aget buffer i) 128) h2))))
        (doto ctx
          (.lineTo w h2)
          (.stroke)))
      (js/requestAnimationFrame #(draw canvas)))))
