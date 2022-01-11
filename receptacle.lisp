(in-package #:org.shirakumo.fraf.vpetjam)

(define-condition object-not-accepted (error) ())

(defclass receptacle ()
  ())

(defgeneric receive (object receptacle))

(defmethod receive (object (receptacle receptacle))
  (error 'object-not-accepted))

(define-shader-entity object (game-entity)
  ((height :initform 0.0 :accessor height)
   (hvel :initform 0.0 :accessor hvel)))

(defmethod handle :after ((ev tick) (object object))
  (when (< 0 (height object))
    (decf (hvel object) (* (dt ev) 15))
    (incf (height object) (hvel object))
    (when (<= (height object) 0.1)
      (cond ((<= (abs (hvel object)) 0.1)
             (setf (height object) 0.0)
             (setf (hvel object) 0.0)
             (vsetf (velocity object) 0 0))
            (T
             (nv* (velocity object) 0.5)
             (setf (hvel object) (* (hvel object) -0.5))
             (setf (height object) 0.1))))
    (bvh:do-fitting (entity (bvh +world+) object)
      (when (typep entity 'receptacle)
        (handler-case (receive object entity)
          (object-not-accepted ()))))
    (nv+ (frame-velocity object) (velocity object))))

(defmethod apply-transforms progn ((object object))
  (translate-by 0 (height object) 0))

(defmethod receive :after ((object object) (receptacle receptacle))
  (when (slot-boundp object 'container)
    (leave* object T)))
