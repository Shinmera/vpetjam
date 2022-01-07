(in-package #:org.shirakumo.fraf.vpetjam)

(define-asset (vpetjam farm-mesh) mesh
    (make-rectangle 1920 1080))

(define-shader-entity farm (vertex-entity textured-entity located-entity)
  ((name :initform 'farm)
   (vertex-array :initform (// 'vpetjam 'farm-mesh))
   (texture :initform (// 'vpetjam 'farm))
   (location :initform (vec 0 0 10000))))

(defmethod bsize ((farm farm))
  (load-time-value (vec (/ 1920 2) (/ 1080 2))))
