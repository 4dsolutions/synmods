(ns test-project.synmods)

(defn add-open
  [a2 b2 c2 d2 e2 f2]
  (+ (* f2 a2 b2)
     (* d2 a2 c2)
     (* a2 b2 e2)
     (* c2 b2 d2)
     (* e2 c2 a2)
     (* f2 c2 b2)
     (* e2 d2 a2)
     (* b2 d2 f2)
     (* b2 e2 f2)
     (* d2 e2 c2)
     (* a2 f2 e2)
     (* d2 f2 c2)))

(defn add-closed
  [a2 b2 c2 d2 e2 f2]
  (+ (* a2 b2 d2)(* d2 e2 f2)(* b2 c2 e2)(* a2 c2 f2)))

(defn add-opposite
   [a2 b2 c2 d2 e2 f2]
   (+ (* a2 e2 (+ a2 e2)) (* b2 f2 (+ b2 f2))(* c2 d2 (+ c2 d2))))

(defn Volume
   [edges]
   (let [[a2 b2 c2 d2 e2 f2] (map (fn [x] (* x x)) edges )]
     (Math/sqrt (*
                  (-
                    (- (add-open a2 b2 c2 d2 e2 f2)(add-closed a2 b2 c2 d2 e2 f2) )
                    (add-opposite a2 b2 c2 d2 e2 f2))
                  0.5))))

(println (format "All edges D=1, Volume: %s" (Volume [1.0 1.0 1.0 1.0 1.0 1.0]) ))

; A Module
; Fig. 986.421
; http://www.rwgrayprojects.com/synergetics/s09/figs/f86421.html

(def a 1.0)
(def EF (* a (/ (Math/sqrt 6.0) 12.0 )))
(def EC (* a (/ (Math/sqrt 6.0) 4.0 )))
(def ED (* a (/ (Math/sqrt 2.0) 4.0 )))
(def FC (* a (/ (Math/sqrt 3.0) 3.0 )))
(def CD (/ a 2.0) )
(def DF (* a (/ (Math/sqrt 3.0) 6.0 )))

(def Avol (Volume [EF EC ED FC CD DF]))
(println (format "Amod volume: %s" Avol))

; E Module
; Fig. 986.411A T & E Module
; http://www.rwgrayprojects.com/synergetics/s09/figs/f86411a.html

(def D 1.0)
(def R (/ D 2.0))
(def h R)

(def OC h)
(def OA (* h (Math/sqrt (/ (- 5.0 (Math/sqrt 5.0)) 2.0))) )
(def OB (* h (Math/sqrt (/ (- 9.0 (* 3 (Math/sqrt 5.0))) 2.0))))
(def CA (* (/ h 2.0) (- (Math/sqrt 5.0) 1.0)))
(def AB (* h (Math/sqrt (- 5.0 (* 2.0 (Math/sqrt 5.0))))))
(def BC (* (/ h 2.0) (- 3.0 (Math/sqrt 5.0))))

(def Evol (Volume [OC OA OB CA AB BC]))
(println (format "Emod volume: %s" Evol))

; S Module
; Fig. 988.13A S Quanta Module Edge Lengths
; http://www.rwgrayprojects.com/synergetics/s09/figs/f8813a.html

(def a D)
(def FG (* (/ a 2.0) (*  (Math/sqrt 3.0) (Math/sqrt (- 7.0 (* 3.0 ( Math/sqrt 5.0))))) ) )
(def FE (* a (Math/sqrt (- 7.0 (* 3.0 (Math/sqrt 5))))))
(def FH (* (/ a 2.0) (- (Math/sqrt 5.0) 1.0)))
(def GE (* (/ a 2.0) (Math/sqrt (- 7.0 (* 3.0 ( Math/sqrt 5))))))
(def EH (* (/ a 2.0) (- 3.0 (Math/sqrt 5.0))))
(def HG GE)

(def Svol (Volume [FG FE FH GE EH HG]))
(println (format "Smod volume: %s" Svol))

(println (format "sFactor: %s" (/ Svol Evol)))
