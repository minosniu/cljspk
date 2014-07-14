(ns neurons.core
  ;(:require [clojure.core.reducers :as r])
  (:use [overtone.at-at :only (mk-pool stop every after)])
  (:use [riemann.client :only (udp-client tcp-client send-event query)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ant sim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(def running true)
(def thread-pool-neuron (mk-pool))
;dimensions of square world
;(def dim 40)
(def dim-x 64)
(def dim-y 48)
;number of ants = nants-sqrt^2
(def nants-sqrt 1)
;scale factor for membrane potential drawing
(def green-scale 0.5)
;pixels per world cell
(def scale 15)

;start riemann server
;(def riemann-server (tcp-client :host "127.0.0.1"))
(def riemann-server (tcp-client :host "192.168.0.114"))

(def animation-sleep-ms 16)
(def watcher-sleep-ms 32)
(def fly-sleep-ms 80)

;dimension of the receptive field
(def dim-field 8)

(def fx (atom 30))
(def fy (atom 30))

;neuron refresh rate in ms, but essential sampling rate is always 1ms
(def period-ms 1)

(defstruct neuron-struct :v :u) 

;neuron-grid is a 2d vector of refs to neurons
(def neuron-grid 
  (apply vector 
         (map (fn [_] 
                (apply vector (map (fn [_] (ref (struct neuron-struct -70.0 -14.0))) 
                                   (range dim-y)))) 
              (range dim-x))))

(def synaptic-currents 
  (apply vector 
         (map (fn [_] 
                (apply vector (map (fn [_] (atom 0.0)) 
                                   (range dim-y)))) 
              (range dim-x))))

(defn syn-i [[x y]]
  (-> synaptic-currents (nth x) (nth y)))

(defn neuron [[x y]]
  (-> neuron-grid (nth x) (nth y)))

(defn bound 
  "returns n wrapped into range 0-b"
  [b n]
  (min (dec b) n))

;(defmacro dosync [& body]
;  `(sync nil ~@body))

(defn set-fly [[new_fx new_fy]]
  (dosync 
    (reset! fx new_fx)
    (reset! fy new_fy)))

(defn move-fly-nosleep []
  (dosync
    (let [new_x (bound dim-x (+ @fx (dec (rand 2))))
          new_y (bound dim-y (+ @fx (dec (rand 2))))]
    (set-fly [new_x new_y]))))

(defn move-fly [_]
  (move-fly-nosleep)
  (Thread/sleep fly-sleep-ms)
  (when running
    (send-off *agent* #'move-fly))
  nil)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import 
 '(java.awt Color Graphics Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))


(defn fill-cell [#^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-ant [ant #^Graphics g x y]
  (let [black (. (new Color 0 0 0 255) (getRGB))
        gray (. (new Color 100 100 100 255) (getRGB))
        red (. (new Color 255 0 0 255) (getRGB))
        [hx hy tx ty] ({0 [2 0 2 4] 
                        1 [4 0 0 4] 
                        2 [4 2 0 2] 
                        3 [4 4 0 0] 
                        4 [2 4 2 0] 
                        5 [0 4 4 0] 
                        6 [0 2 4 2] 
                        7 [0 0 4 4]}
                       (:dir ant))]
    (doto g
      (.setColor (if (:food ant) 
                  (new Color 255 0 0 255) 
                  (new Color 0 0 0 255)))
      (.drawLine (+ hx (* x scale)) (+ hy (* y scale)) 
                (+ tx (* x scale)) (+ ty (* y scale))))))

(defn render-place [g p x y]
  (let [potential (+ (:v p) 70.0)]
  ;(let [potential (if (= x 10) 30.0 0.0)]
    (when (pos? potential)
	    (fill-cell g x y (new Color 255 0 0 
	                          (int (min 255 (/ potential green-scale))))))
	  (when (:ant p)
	    (render-ant (:ant p) g x y))))

(defn render [g]
  (let [v (dosync (apply vector (for [x (range dim-x) y (range dim-y)] 
                                   @(neuron [x y]))))
        img (new BufferedImage (* scale dim-x) (* scale dim-y) 
                 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (doseq [x (range dim-x) y (range dim-y)]
       (render-place bg (v (+ (* x dim-y) y)) x y))
    (doto bg
      (.setColor (. Color blue))
      )
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(def panel (doto (proxy [JPanel] []
                        (paint [g] (render g)))
             (.setPreferredSize (new Dimension 
                                     (* scale dim-x) 
                                     (* scale dim-y)))))

(def frame (doto (new JFrame) (.add panel) .pack .show))

(def animator (agent nil))

(defn animation [_]
  (. panel (repaint))
  (. Thread (sleep animation-sleep-ms))
  (when running
    (send-off *agent* #'animation))
  nil)


;;;;;;;;;;;;;;; Izhikevich Neuron ;;;;;;;;;;;;;;;;;;;;;;;;;;
; Use overtone/at.at to schedule the update of izh-output
; However izh-input is event-based
; First have 1 ant wander around, wherever it goes, send synaptic drive to surrounding neurons

(defn izh-full [a b c d {u :u v :v} i [x y]]
;;;;;;;Izhikevich in Python
; def integrate(self, n=None):
;     if n is None: n = self.neuron
;     trace = zeros((2,len(self.t)))
;     for i, j in enumerate(self.stim):
;       n.v += self.dt * (0.04*n.v**2 + self.x*n.v + self.y - n.u + self.stim[i])
;       n.u += self.dt * self.du(n.a,n.b,n.v,n.u)
;       if n.v > 30:
;         trace[0,i] = 30
;         n.v        = n.c
;         n.u       += n.d
;       else:
;         trace[0,i] = n.v
;         trace[1,i] = n.u
;     return trace

;    self.du     = lambda a, b, v, u: a*(b*v - u)
  (let [t1 (* 0.04 v v)
        t2 (* 5.0 v)
        t3 140.0
        t4 (- u)
        dt 1.0 ;in ms
        du (* a (- (* b v) u))
        v_new (+ v (* dt (+ t1 t2 t3 t4 i)))
        u_new (+ u (* dt du))
        tagx (if (< x 10) (str 0 x) (str x))
        tagy (if (< y 10) (str 0 y) (str y))]
    (if (> v_new 30.0)
      (do
        ;(send-event riemann-server {:host tagx :service tagy :state "critical"})   
        ;(after 1 (send-event riemann-server {:host tagx :service tagy :state "ok"}) thread-pool-neuron)
        {:v c :u (+ u_new d)})
      {:v v_new :u u_new})))

(def a 0.02)
(def b 0.2)
(def c -65.0)
(def d 2.0)

(def integrate-izh (partial izh-full a b c d))

(defn refresh-all-neurons-full [n]
  (let []
    ;(println "update " n "times")
    (dosync
      (dotimes [i n]
        ;Assumes Izh model needs 1kHz samping rate, so run #period-in-ms updates per cycle
        (dorun
          (for [x (range dim-x) y (range dim-y)]
            (let [loc [x y]
                  new_uv (integrate-izh @(neuron loc) @(syn-i loc))]
              (alter (neuron loc) merge new_uv )
              )))))))

(def refresh-all-neurons-nosleep (partial refresh-all-neurons-full period-ms))

(defn refresh-all-neurons [_]
  (refresh-all-neurons-nosleep)
  (Thread/sleep period-ms)
  (when running
    (send *agent* #'refresh-all-neurons))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;demo
;(dorun (map #(send-off % behave) ants))
;(every watcher-sleep-ms #(send-off (first ants) behave-nosleep) thread-pool-neuron)


(defn watch-fly-nosleep []
  (dosync 
    (doseq [x (range dim-x)
            y (range dim-y)]
        (reset! (syn-i [x y]) 0.0))
    (dotimes [i (* dim-field dim-field)]
      (let [x (bound dim-x (+ @fx (int (rand dim-field))))
            y (bound dim-y (+ @fy (int (rand dim-field))))
            dist (+ (* (- x @fx) (- x @fx)) (* (- y @fy) (- y @fy)))
            ]
        (reset! (syn-i [x y]) (* 100 (rand dist)))))))

(defn watch-fly [x]
  (watch-fly-nosleep)
  (Thread/sleep watcher-sleep-ms)
  (when running
    (send-off *agent* #'watch-fly))
  nil)

(def watcher (agent nil))
(def fly (agent nil))
(def refresher (agent nil))

;(every period-ms refresh-all-neurons-nosleep thread-pool-neuron)
;(dorun (map #(send-off % behave) ants))
;(every watcher-sleep-ms #'watch-fly-nosleep thread-pool-neuron)

(def slicers
  (map #(agent %) (range dim-x)))

(defn zip
  [& colls]
  (apply map vector colls))

(defn refresh-slice-nosleep [uv-list syn-i-list n]
  (let []
    (dosync
      (dotimes [i n]
        ;Assumes Izh model needs 1kHz samping rate, so run #period-in-ms updates per cycle
        (doseq [[uv syn-i] (zip uv-list syn-i-list)]
            (let [;loc [x y]
                  new_uv (integrate-izh @uv @syn-i)]
              (alter uv merge new_uv )
              ))))))

(defn refresh-slice-nosleep-xy [x n]
  (let [uv-list (neuron-grid x)
        syn-i-list (synaptic-currents x)]
    (dosync
      (dotimes [i n]
        ;Assumes Izh model needs 1kHz samping rate, so run #period-in-ms updates per cycle
        (doseq [y (range dim-y)]
            (let [uv (uv-list y)
                  syn-i (syn-i-list y)
                  new_uv (integrate-izh @uv @syn-i [x y])]
              (alter uv merge new_uv )
              ))))))

(defn refresh-slice [_]
  ;(refresh-slice-nosleep (neuron-grid @*agent*) (synaptic-currents @*agent*) period-ms)
  (refresh-slice-nosleep-xy @*agent* period-ms)
  (Thread/sleep period-ms)
  (when running
    (send *agent* #'refresh-slice))
  @*agent*)


(send-off animator animation)
(set-fly [10 10])
(dorun (map #(send % refresh-slice) slicers))
;(send refresher refresh-all-neurons)
(send-off fly move-fly)
(send-off watcher watch-fly)