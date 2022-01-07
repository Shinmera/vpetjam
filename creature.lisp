(in-package #:org.shirakumo.fraf.vpetjam)

(define-asset (vpetjam creature-mesh) mesh
    (make-rectangle 128 128))

(define-shader-entity creature (vertex-entity textured-entity game-entity listener)
  ((name :initform (generate-name 'creature))
   (vertex-array :initform (// 'vpetjam 'creature-mesh))
   (texture :initform (// 'vpetjam 'creature))))

(defmethod apply-transforms progn ((creature creature))
  (translate-by 0 +92 0))

(define-shader-entity crop (animated-sprite)
  ((name :initform (generate-name 'crop)))
  (:default-initargs :sprite-data (asset 'vpetjam 'crop)))

(defmethod switch-animation :after ((crop crop) next)
  (enter-and-load (make-instance 'creature :location (location crop)) +world+ +main+)
  (leave* crop +world+))
