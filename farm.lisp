(in-package #:org.shirakumo.fraf.vpetjam)

(define-shader-entity farm (basic-entity)
  ((name :initform 'farm)
   (texture :initform (// 'vpetjam 'farm))
   (bsize :initform (vec (/ 1920 2) (/ 1080 2)))
   (location :initform (vec 0 0 10000))))

(define-shader-entity plot (basic-entity)
  ((texture :initform (// 'vpetjam 'plot))
   (bsize :initform (vec 256 256))))

(defmethod enter :after ((plot plot) (container flare:container))
  (let ((s (+ 64 128)))
    (loop for y from (- s) to (+ s) by 128
          do (loop for x from (- s) to (+ s) by 128
                   do (enter (make-instance 'spot :location (v- (location plot) (vec x y y))) container)))))

(define-shader-entity seed (part object)
  ((texture :initform (// 'vpetjam 'seed))
   (bsize :initform (vec 32 32))))

(defclass spot (game-entity receptacle)
  ((bsize :initform (vec 64 64))
   (holding :initform NIL :accessor holding)))

(defmethod object-accepted-p ((seed seed) (spot spot))
  (and (= 0 (height seed))
       (null (holding spot))))

(defmethod receive ((seed seed) (spot spot))
  (maybe-leave seed)
  (let ((crop (make-instance 'crop :parent spot :location (vcopy (location spot)))))
    (setf (holding spot) crop)
    (enter-and-load crop (container spot) +main+)))

(define-shader-entity sell (basic-receptacle)
  ((texture :initform (// 'vpetjam 'sell))
   (bsize :initform (vec 64 32))))

(defmethod object-accepted-p ((object object) (sell sell)) T)
(defmethod receive ((object object) (sell sell)))

(define-shader-entity combine (basic-receptacle)
  ((texture :initform (// 'vpetjam 'combine))
   (bsize :initform (vec 48 48))
   (holding :initform NIL :accessor holding)
   (work-time :initform 0.0 :accessor work-time)
   (state :initform :empty :accessor state)))

(defmethod object-accepted-p ((seed seed) (combine combine))
  (not (eql :working (state combine))))

(defmethod receive ((seed seed) (combine combine))
  (ecase (state combine)
    (:empty
     (setf (holding combine) seed)
     (setf (state combine) :holding))
    (:holding
     (let* ((left (shiftf (holding combine) NIL))
            (right seed)
            (result (make-instance 'seed :location (vcopy (location combine))
                                         :height 32 :hvel 1.0 :velocity (vec 0 -8))))
       (setf (holding combine) result)
       (setf (state combine) :working)))))

(defmethod handle ((ev tick) (combine combine))
  (case (state combine)
    (:working
     (let ((tt (incf (work-time combine) (dt ev))))
       (cond ((<= 1.2 tt)
              (setf (work-time combine) 0.0)
              (setf (state combine) :empty))
             ((and (<= 1.0 tt) (holding combine))
              (enter-and-load (holding combine) (container combine) +main+)
              (setf (holding combine) NIL)))))))

(defmethod apply-transforms progn ((combine combine))
  (when (and (< 0.5 (bulge-time combine)) (< 0 (work-time combine)))
    (translate-by (random* 0 0.1) (random* 0 0.1) 0)))

(define-shader-entity crop (animated-sprite)
  ((name :initform (generate-name 'crop))
   (parent :initarg :parent :accessor parent))
  (:default-initargs :sprite-data (asset 'vpetjam 'crop)))

(defmethod switch-animation :after ((crop crop) next)
  (enter-and-load (make-instance 'creature :location (location crop)) +world+ +main+)
  (leave* crop +world+)
  (setf (holding (parent crop)) NIL))

(defmethod apply-transforms progn ((crop crop))
  (translate-by 0 -64 0))
