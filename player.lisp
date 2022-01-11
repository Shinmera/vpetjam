(in-package #:org.shirakumo.fraf.vpetjam)

(define-shader-entity cursor (vertex-entity colored-entity sized-entity)
  ((vertex-array :initform (// 'vpetjam '64x))
   (color :initform (vec 1 0 0 0.5))
   (bsize :initform (vec 32 32))))

(define-shader-entity player (part-parent playhead facing-entity game-entity)
  ((name :initform 'player)
   (stack :initform () :accessor stack)
   (uv-offset :initform (vec 0 6))
   (action-playhead :initform (make-instance 'playhead) :accessor action-playhead)
   (cursor :initform (make-instance 'cursor) :accessor cursor))
  (:default-initargs :children '((:head  :uv (0 7) :pivot (0 -18) :children ((:face :uv (0 1) :location (0   0 -1))))
                                 (:lleg  :uv (0 5) :pivot (0  28) :children ((:foot :uv (0 4) :location (0 -32 +1))))
                                 (:rleg  :uv (0 5) :pivot (0  28) :children ((:foot :uv (0 4) :location (0 -32 +1))))
                                 (:larm  :uv (0 3) :pivot (0  19) :children ((:hand :uv (0 2) :location (0 -30 +1))))
                                 (:rarm  :uv (0 3) :pivot (0  19) :children ((:hand :uv (0 2) :location (0 -30 +1)))))))

(defmethod initialize-instance :after ((player player) &key)
  (start-loop 'player-stand-forward player))

(define-clip player-stand-forward
  0.0 (((:head location) (vec   0  50 -1)) ((:head angle) 0.0) ((:head skew) (vec +1 +1))
       ((:lleg location) (vec -14 -58 +1)) ((:lleg angle) 0.0) ((:lleg skew) (vec +1 +1))
       ((:rleg location) (vec +14 -58 +1)) ((:rleg angle) 0.0) ((:rleg skew) (vec -1 +1))
       ((:larm location) (vec -20   0 +1)) ((:larm angle) 0.0) ((:larm skew) (vec +1 +1))
       ((:rarm location) (vec +20   0 +1)) ((:rarm angle) 0.0) ((:rarm skew) (vec -1 +1)))
  1.0 (((:head location) (vec   0 48 -1)) ((:larm angle) -0.2) ((:rarm angle) -0.2))
  2.0 (((:head location) (vec   0 50 -1)) ((:larm angle) 0.0) ((:rarm angle) 0.0)))

(define-clip player-stand-side
  0.0 (((:head location) (vec  -2  55 -1)) ((:head angle) 0.0) ((:head skew) (vec +1 +1))
       ((:lleg location) (vec  -5 -60 -1)) ((:lleg angle) 0.0) ((:lleg skew) (vec +1 +1))
       ((:rleg location) (vec  +5 -45 +1)) ((:rleg angle) 0.0) ((:rleg skew) (vec +1 +1))
       ((:larm location) (vec  -2  -5 -2)) ((:larm angle) +0.2) ((:larm skew) (vec +1 +1))
       ((:rarm location) (vec  +2 +10 +1)) ((:rarm angle) -0.2) ((:rarm skew) (vec +1 +1)))
  1.0 (((:head location) (vec  -2  50 -1)) ((:larm angle) -0.2) ((:rarm angle) +0.2))
  2.0 (((:head location) (vec  -2  55 -1)) ((:larm angle) +0.2) ((:rarm angle) -0.2)))

(define-clip player-walk-side
  0.0 (((:head location) (vec  -2  50 -1)) ((:head angle) 0.0) ((:head skew) (vec +1 +1))
       ((:lleg location) (vec  -5 -60 -1)) ((:lleg angle) -0.75) ((:lleg skew) (vec +1 +0.7))
       ((:rleg location) (vec  +5 -45 +1)) ((:rleg angle) +0.75) ((:rleg skew) (vec +1 +1))
       ((:larm location) (vec  -2  -5 -2)) ((:larm angle) +0.6) ((:larm skew) (vec +1 +1))
       ((:rarm location) (vec  +2 +10 +1)) ((:rarm angle) -0.6) ((:rarm skew) (vec +1 +1)))
  0.2 (((:rleg angle)  0.0) ((:rleg skew) (vec +1 +1.0))
       ((:head location) (vec  -2  55 -1)))
  0.5 (((:lleg angle) +0.75)
       ((:rleg angle) -0.75) ((:rleg skew) (vec +1 0.7))
       ((:larm angle) -0.6) ((:rarm angle) +0.6)
       ((:head location) (vec  -2  50 -1)))
  0.7 (((:lleg angle)  0.0) ((:lleg skew) (vec +1 +1.0))
       ((:head location) (vec  -2  55 -1)))
  1.0 (((:lleg angle) -0.75) ((:lleg skew) (vec +1 0.7))
       ((:rleg angle) +0.75) ((:rleg skew) (vec +1 1.0))
       ((:larm angle) +0.6) ((:rarm angle) -0.6)
       ((:head location) (vec  -2  50 -1))))

(define-clip player-walk-forward
  0.0 (((:head location) (vec   0  50 -1)) ((:head angle) 0.0) ((:head skew) (vec +1 +1))
       ((:lleg location) (vec -14 -58 +1)) ((:lleg angle) +0.07) ((:lleg skew) (vec +1 +0.5))
       ((:rleg location) (vec +14 -58 +1)) ((:rleg angle) 0.0) ((:rleg skew) (vec -1 +1))
       ((:larm location) (vec -20   0 -1)) ((:larm angle) +0.1) ((:larm skew) (vec +1 +1))
       ((:rarm location) (vec +20   0 +1)) ((:rarm angle) +0.1) ((:rarm skew) (vec -1 +0.5)))
  0.2 (((:rleg skew) (vec -1 0.8)) ((:rleg angle) +0.15)
       ((:rleg skew) (vec -1 0.75)) ((:rleg angle) 0)
       ((:larm angle) -0.1)
       ((:rarm angle) -0.1)
       ((:head location) (vec   0 45 -1)))
  0.5 (((:rleg skew) (vec -1 0.5))
       ((:lleg skew) (vec +1 +1)) ((:lleg angle) +0.0)
       ((:larm location) (vec -20   0 +1)) ((:larm angle) +0.1) ((:larm skew) (vec +1 +0.5))
       ((:rarm location) (vec +20   0 -1)) ((:rarm angle) +0.1) ((:rarm skew) (vec -1 +1.0))
       ((:head location) (vec   0 50 -1)))
  0.7 (((:rleg skew) (vec -1 0.75)) ((:rleg angle) 0)
       ((:lleg skew) (vec +1 0.8)) ((:lleg angle) +0.15)
       ((:larm angle) -0.1)
       ((:rarm angle) -0.1)
       ((:head location) (vec   0 45 -1)))
  1.0 (((:rleg skew) (vec -1 +1)) ((:rleg angle) +0.0)
       ((:lleg skew) (vec +1 +0.5)) ((:lleg angle) +0.07)
       ((:larm location) (vec -20   0 -1)) ((:larm angle) +0.1) ((:larm skew) (vec +1 +1))
       ((:rarm location) (vec +20   0 +1)) ((:rarm angle) +0.1) ((:rarm skew) (vec -1 +0.5))
       ((:head location) (vec   0 50 -1))))

(define-clip player-pickup-forward
  0.0 (((:larm location) (vec -20   0 -1)) ((:larm angle) +0.1) ((:larm skew) (vec +1 +1))
       ((:rarm location) (vec +20   0 +1)) ((:rarm angle) +0.1) ((:rarm skew) (vec -1 +1)))
  0.3 (((:larm location) (vec -20   0 -1)) ((:larm angle) +0.1) ((:larm skew) (vec +1 -1))
       ((:rarm location) (vec +20   0 +1)) ((:rarm angle) +0.1) ((:rarm skew) (vec -1 -1))))

(define-clip player-pickup-side
  0.0 (((:larm location) (vec  -2  -5 -2)) ((:larm angle) +0.7) ((:larm skew) (vec +1 +1))
       ((:rarm location) (vec  +2 +10 +1)) ((:rarm angle) +0.7) ((:rarm skew) (vec +1 +1)))
  0.3 (((:larm angle) +3.3)
       ((:rarm angle) +3.1)))

(defmethod (setf field) (value field (player player))
  (destructuring-bind (part func) field
    (let ((part (part part player)))
      (ecase func
        (location (setf (location part) value))
        (angle (setf (angle part) value))
        (skew (setf (skew part) value))))))

(defmethod field (field (player player))
  (destructuring-bind (part func) field
    (let ((part (part part player)))
      (ecase func
        (location (location part))
        (angle (angle part))
        (skew (skew part))))))

(defmethod (setf direction) :after (dir (player player))
  (setf (slot-value player 'direction) dir))

(defmethod enter :after ((player player) (container flare:container))
  (enter (cursor player) container))

(defmethod leave :after ((player player) (container flare:container))
  (leave (cursor player) container))

(defmethod handle ((ev tick) (player player))
  (call-next-method)
  (let ((vel (velocity player))
        (dt (dt ev))
        (spd 6)
        (acc 15)
        (dcc 25))
    (cond ((retained 'left)
           (if (<= (vx vel) (- spd))
               (setf (vx vel) (- spd))
               (decf (vx vel) (* dt acc))))
          ((retained 'right)
           (if (<= spd (vx vel))
               (setf (vx vel) spd)
               (incf (vx vel) (* dt acc))))
          (T
           (if (<= (abs (vx vel)) (* dcc dt))
               (setf (vx vel) 0.0)
               (decf (vx vel) (float-sign (vx vel) (* dcc dt))))))
    (cond ((retained 'down)
           (if (<= (vy vel) (- spd))
               (setf (vy vel) (- spd))
               (decf (vy vel) (* dt acc))))
          ((retained 'up)
           (if (<= spd (vy vel))
               (setf (vy vel) spd)
               (incf (vy vel) (* dt acc))))
          (T
           (if (<= (abs (vy vel)) (* dcc dt))
               (setf (vy vel) 0.0)
               (decf (vy vel) (float-sign (vy vel) (* dcc dt))))))
    (nv+ (frame-velocity player) vel))
  (setf (location (cursor player))
        (nvalign (v+ (location player) (vxy_ (v* (direction player) 96))) 64))
  (decf (vy (location (cursor player))) 64)
  (setf (vz (location (cursor player))) (vy (location (cursor player)))))

(defmethod handle :after ((ev tick) (player player))
  (let ((vel (velocity player)))
    (cond ((or (/= 0 (vx vel)) (/= 0 (vy vel)))
           (cond ((< (abs (vy vel)) (abs (vx vel)))
                  (setf (direction player) (vec (float-sign (vx vel)) 0.0))
                  (start-loop 'player-walk-side player))
                 ((< (vy vel) 0)
                  (setf (direction player) (vec 0.0 -1.0))
                  (start-loop 'player-walk-forward player))
                 ((< 0 (vy vel))
                  (setf (direction player) (vec 0.0 +1.0))
                  (start-loop 'player-walk-forward player))))
          ((< 0 (vy (direction player)))
           (start-loop 'player-stand-forward player))
          ((< (vy (direction player)) 0)
           (start-loop 'player-stand-forward player))
          (T
           (start-loop 'player-stand-side player)))
    (setf (clip (action-playhead player))
          (when (stack player)
            (if (= 0 (vx (direction player))) 'player-pickup-forward 'player-pickup-side)))
    (advance (action-playhead player) player (dt ev))
    (setf (render-list player) (sort (alexandria:hash-table-values (children player)) #'>
                                     :key (lambda (p) (vz (location p)))))))

(defmethod handle ((ev interact) (player player))
  (let* ((cursor (cursor player))
         (found (bvh:do-fitting (entity (bvh +world+) cursor)
                  (unless (eql entity player)
                    (return entity)))))
    (typecase found
      (creature
       (setf (location found) (vec 0 -50 +1))
       (setf (angle found) 0)
       (push found (stack player))
       (start-clip 'player-pickup-side (action-playhead player))
       (reset (action-playhead player))
       (leave* found T))
      (null
       (if (<= (vlength (velocity player)) 0.1)
           (loop for entity in (stack player)
                 do (setf (location entity) (location cursor)))
           (loop for entity in (stack player)
                 do (setf (location entity) (location player))
                    (setf (height entity) 120)
                    (setf (hvel entity) 2.0)
                    (setf (velocity entity) (v* (direction player) 10.0))))
       (loop for entity = (pop (stack player))
             while entity
             do (setf (direction entity) (vcopy (direction player)))
                (enter* entity (container player))))
      (interactable
       (when (stack player)
         )))))

(defmethod render :after ((player player) (program shader-program))
  (when (eql :stopped (state (action-playhead player)))
    (translate-by 0 150 0)
    (dolist (entity (stack player))
      (render-child entity program)
      (translate-by (* (vx (velocity player)) -2 (vx (direction player)))
                    (+ 50 (* (vy (velocity player)) -2))
                    0))))
