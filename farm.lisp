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

(defmethod receive :before ((object object) (spot spot))
  (when (or (< 0 (height object))
            (holding spot))
    (error 'object-not-accepted)))

(defmethod receive ((seed seed) (spot spot))
  (print (list spot (location spot)))
  (maybe-leave seed)
  (let ((crop (make-instance 'crop :parent spot :location (vcopy (location spot)))))
    (setf (holding spot) crop)
    (enter-and-load crop (container spot) +main+)))

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
