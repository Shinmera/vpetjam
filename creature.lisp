(in-package #:org.shirakumo.fraf.vpetjam)

(define-shader-entity object (game-entity)
  ((height :initform 0.0 :accessor height)
   (hvel :initform 0.0 :accessor hvel)))

(defmethod handle :after ((ev tick) (object object))
  (when (< 0 (height object))
    (decf (hvel object) (* (dt ev) 15))
    (incf (height object) (hvel object))
    (when (<= (height object) 0.0)
      (cond ((<= (abs (hvel object)) 0.1)
             (setf (height object) 0.0)
             (setf (hvel object) 0.0)
             (vsetf (velocity object) 0 0))
            (T
             (nv* (velocity object) 0.5)
             (setf (hvel object) (* (hvel object) -0.5))
             (setf (height object) 0.1))))
    (nv+ (frame-velocity object) (velocity object))))

(defmethod apply-transforms progn ((object object))
  (translate-by 0 (height object) 0))

(define-shader-entity creature (part-parent object)
  ((name :initform (generate-name 'creature))
   (texture :initform (// 'vpetjam 'creature))))

(defmethod apply-transforms progn ((creature creature))
  (scale-by 1.5 1.5 1))

(defmethod (setf direction) (dir (creature creature)))

(define-shader-entity crop (animated-sprite)
  ((name :initform (generate-name 'crop)))
  (:default-initargs :sprite-data (asset 'vpetjam 'crop)))

(defmethod switch-animation :after ((crop crop) next)
  (enter-and-load (make-instance 'creature :location (location crop)) +world+ +main+)
  (leave* crop +world+))
