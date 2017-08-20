(ns lunar-lander.core
  (:import [java.text NumberFormat]
           [org.encog.ml.data.basic BasicMLData]
           [org.encog.ml CalculateScore]
           [org.encog.ml MethodFactory]
           [org.encog.ml.genetic MLMethodGeneticAlgorithm]
           [org.encog.neural.networks.training.anneal NeuralSimulatedAnnealing]
           [org.encog.engine.network.activation ActivationTANH]
           [org.encog.neural.pattern FeedForwardPattern]
           [org.encog.util.arrayutil NormalizedField NormalizationAction]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LanderSimulator

(def GRAVITY 1.62)
(def THRUST 10)
(def TERMINAL-VELOCITY 40)

(defprotocol PLanderSimulator
  (ls-set-defaults [this])
  (get-fuel [this])
  (get-seconds [this])
  (get-altitude [this])
  (get-velocity [this])
  (turn [this thrust?])
  (telemetry [this])
  (flying? [this])
  (score [this]))

(deftype LanderSimulator [^:volatile-mutable fuel
                          ^:volatile-mutable seconds
                          ^:volatile-mutable altitude
                          ^:volatile-mutable velocity]
  PLanderSimulator
  (ls-set-defaults [this]
    (set! fuel 200)(set! seconds 0)(set! altitude 10000)(set! velocity 0)
    this)
  (get-fuel [this] fuel)
  (get-seconds [this] seconds)
  (get-altitude [this] altitude)
  (get-velocity [this] velocity)
  (turn [this thrust?]
    (set! seconds (inc seconds))
    (set! velocity (- velocity GRAVITY))
    (set! altitude (+ altitude velocity))
    (when (and thrust? (pos? fuel))
      (set! fuel (dec fuel))
      (set! velocity (+ velocity THRUST)))
    (set! velocity (min TERMINAL-VELOCITY (max (- TERMINAL-VELOCITY) velocity)))
    (when (neg? altitude) (set! altitude 0))
    this)
  (flying? [this] (pos? altitude))
  (score [this] (+ (* 10 fuel) seconds (* 1000 velocity)))
  (telemetry [this]
    (let [nf (NumberFormat/getNumberInstance)]
      (.setMinimumFractionDigits nf 4)
      (.setMaximumFractionDigits nf 4)
      (str "Elapsed: " seconds "s, Fuel: " fuel
           " l, Velocity: " (.format nf velocity) " m/s, "
           (int altitude) " m"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NeuralPilot

(defprotocol PNeuralPilot
  (np-set-defaults [this])
  (scorePilot [this]))

(deftype NeuralPilot [network track?
                      ^:volatile-mutable f-stats
                      ^:volatile-mutable a-stats
                      ^:volatile-mutable v-stats]
  PNeuralPilot
  (np-set-defaults [this]
    (let [tv TERMINAL-VELOCITY]
      (set! f-stats (NormalizedField. NormalizationAction/Normalize
                                      "fuel" 200 0 -0.9 0.9))
      (set! a-stats (NormalizedField. NormalizationAction/Normalize
                                      "altitude" 10000 0 -0.9 0.9))
      (set! v-stats (NormalizedField. NormalizationAction/Normalize
                                      "velocity" tv (- tv) -0.9 0.9)))
    this)
  (scorePilot [this]
    (let [sim (.ls-set-defaults (LanderSimulator. nil nil nil nil))]
      (loop []
        (if-not (.flying? sim)
          (.score sim)
          (let [input (BasicMLData. 3)
                value (.getData (.compute network
                                          (doto input
                                            (.setData 0 (.normalize f-stats (.get-fuel sim)))
                                            (.setData 1 (.normalize a-stats (.get-altitude sim)))
                                            (.setData 2 (.normalize v-stats (.get-velocity sim)))))
                                0)]
            (.turn sim (pos? value))
            (when track?
              (when (pos? value) (println "THRUST"))
              (println (.telemetry sim)))
            (recur)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PilotScore

(deftype PilotScore []
  CalculateScore
  (calculateScore [this network]
    (.scorePilot (np-set-defaults (NeuralPilot. network false nil nil nil))))
  (shouldMinimize [this] false)
  (requireSingleThreaded [this] false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LunarLander

(defn create-network []
  (doto (.generate (doto (FeedForwardPattern.)
                     (.setInputNeurons 3)
                     (.addHiddenLayer 50)
                     (.setOutputNeurons 1)
                     (.setActivationFunction (ActivationTANH.))))
    (.reset)))

(defn main [& anneal?]
  (let [train (if anneal?
                (NeuralSimulatedAnnealing. (create-network) (PilotScore.) 10 2 100)
                (MLMethodGeneticAlgorithm.
                 (reify MethodFactory (factor [this] (create-network)))
                 (PilotScore.)
                 500))]
    (dotimes [i 25]
      (.iteration train)
      (println "Epoch #" (inc i) ", Score: " (.getError train)))
    (.finishTraining train)

    (println "How the winning network landed:")
    (-> train
        .getMethod
        (NeuralPilot. true nil nil nil)
        np-set-defaults
        .scorePilot
        println)))
