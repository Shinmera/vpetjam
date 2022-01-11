(in-package #:org.shirakumo.fraf.vpetjam)

(define-shader-entity creature (part-parent object)
  ((name :initform (generate-name 'creature))
   (texture :initform (// 'vpetjam 'creature))))

(defmethod apply-transforms progn ((creature creature))
  (scale-by 1.5 1.5 1))

(defmethod (setf direction) (dir (creature creature)))
