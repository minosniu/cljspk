(ns demo.AntsApplet2
  (:import
    (javax.swing JApplet))
  (:gen-class
    :post-init post-init
    :extends javax.swing.JApplet))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ant sim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;demo
(load-file "/Users/minos_niu/Code/local-cljspk/src/ants.clj")
(def ants (setup))
(send-off animator animation)
(dorun (map #(send-off % behave) ants))
(send-off evaporator evaporation)
