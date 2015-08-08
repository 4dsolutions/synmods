(ns synmods.qrays)
"""
(cl) K. Urner, MIT License 2015
Python -> Java -> Clojure curriculum
Topic:  Martian Math 
http://wikieducator.org/Martian_Math
http://www.grunch.net/synergetics/quadintro.html
https://en.wikipedia.org/wiki/Quadray_coordinates
"""

(defprotocol VectorOps
  (add [this other])
  (sub [this other])
  (len [this])
  (neg [this])
  (norm [this]))

(defrecord Quadray [OA OB OC OD]
  VectorOps
  (norm [this] (let [[x] [(min OA OB OC OD)]]
       (Quadray. (- OA x) (- OB x) (- OC x) (- OD x))))

  (neg [this] (norm (Quadray. (- OA)(- OB)(- OC) (- OD) )))

  (len [this] (let [[k] [(/ (+ OA OB OC OC) 4)]
                    [t0 t1 t2 t3] [(- OA k) (- OB k) (- OC k) (- OD k)]]
                (* (Math/sqrt 2.0)
                   (Math/sqrt (+  (* t0 t0) (* t1 t1) (* t2 t2) (* t3 t3))))))

  (add [this other]
    (norm (Quadray. (+ OA (:OA other))
                    (+ OB (:OB other))
                    (+ OC (:OC other))
                    (+ OD (:OD other)))))

  (sub [this other] (add this (neg other))))

(defrecord XYZray [OX OY OZ]
   VectorOps
   (norm [this] (this))
   (neg [this](XYZray. (- OX) (- OY) (- OZ)))
   (len [this](Math/sqrt (+ (* OX OX)(* OY OY)(* OZ OZ))))
   (add [this other] (XYZray. (+ OX (:OX other))
                     (+ OY (:OY other))
                     (+ OZ (:OZ other))))
   (sub [this other](add this (neg other))))

(defn qray-to-xyzray 
  [qray]  
  (let [[a] [(:OA qray)]
        [b] [(:OB qray)]
        [c] [(:OC qray)]
        [d] [(:OD qray)]
        [x] [(* (/ 1 (Math/sqrt 2))(+ (- (- a b) c) d))]
        [y] [(* (/ 1 (Math/sqrt 2))(- (+ (- a b) c) d))]
        [z] [(* (/ 1 (Math/sqrt 2))(- (- (+ a b) c) d))]]      
    (XYZray. x y z))) 

(defn xyzray-to-qray 
  [xyzray]
  (let [[x] [(:OX xyzray)]
        [y] [(:OY xyzray)]
        [z] [(:OZ xyzray)]
        [a] [(* (/ 1 (Math/sqrt 2))(+ (if (>= x 0) x 0) (if (>= y 0) y 0 )  (if (>= z 0) z 0)))]
        [b] [(* (/ 1 (Math/sqrt 2))(+ (if (< x 0) (- x) 0) (if (< y 0) (- y) 0 )  (if (>= z 0) z 0)))] 
        [c] [(* (/ 1 (Math/sqrt 2))(+ (if (< x 0) (- x) 0) (if (>= y 0) y 0 )  (if (< z 0) (- z) 0)))] 
        [d] [(* (/ 1 (Math/sqrt 2))(+ (if (>= x 0) x 0) (if (< y 0)(- y) 0 )  (if (< z 0) (- z) 0)))]]
  (norm (Quadray. a b c d))))
  
(let [[A] [(Quadray. 1 0 0 0)]
      [B] [(Quadray. 0 1 0 0)]
      [C] [(Quadray. 0 0 1 0)]
      [D] [(Quadray. 0 0 0 1)]
      [E] [(neg A)]
      [F] [(neg B)]
      [G] [(neg C)]
      [H] [(neg D)]
      [I] [(add A B)]
      [J] [(add A C)]
      [K] [(add A D)]
      [L] [(add B C)]
      [M] [(add B D)]
      [N] [(add C D)]
      [O] [(add I J)]
      [P] [(add I K)]
      [Q] [(add I L)]
      [R] [(add I M)]
      [S] [(add N J)]
      [T] [(add N K)]
      [U] [(add N L)]
      [V] [(add N M)]
      [W] [(add J L)]
      [X] [(add L M)]
      [Y] [(add M K)]
      [Z] [(add K J)]]
      (def critical-points {:A A :B B :C C :D D :E E :F F :G G :H H :I I :J J :K K
                            :L L :M M :N N :O O :P P :Q Q :R R :S S :T T :U U :V V
                            :W W :X X :Y Y :Z Z}))
