(in-package #:org.shirakumo.fraf.vpetjam)

(define-asset (vpetjam creature-mesh) mesh
    (make-rectangle 128 128))

(define-shader-entity creature (vertex-entity textured-entity game-entity)
  ((name :initform (generate-name 'creature))
   (vertex-array :initform (// 'vpetjam 'creature-mesh))
   (texture :initform (// 'vpetjam 'creature))))

(defmethod apply-transforms progn ((creature creature))
  (translate-by 0 +92 0))
