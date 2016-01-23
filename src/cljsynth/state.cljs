(ns cljsynth.state)

(defmulti default-node identity)

(defmethod default-node :oscillator [_]
  {:freq 261.6})




(defn new-id [state]
  (str "node-" (count (:nodes state))))

(defn add-new [state k]
  (update-in state [:nodes] conj (assoc (default-node k)
                                        :id (new-id state))))
