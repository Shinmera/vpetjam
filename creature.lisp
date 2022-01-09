(in-package #:org.shirakumo.fraf.vpetjam)

(define-shader-entity creature (part-parent game-entity)
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
