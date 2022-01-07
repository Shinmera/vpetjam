(in-package #:org.shirakumo.fraf.vpetjam)

(define-asset (vpetjam 1x) mesh
    (make-rectangle 1 1 :align :bottomleft))

(define-asset (vpetjam 16x) mesh
    (make-rectangle 16 16))

(define-asset (vpetjam placeholder) image
    #p"placeholder.png")

(defstruct (hit (:constructor %make-hit (object location &optional (time 0f0) (normal (vec 0 0)))))
  (object NIL)
  (location NIL :type vec2)
  (time 0f0 :type single-float)
  (normal NIL :type vec2))

(defun make-hit (object location &optional (time 0f0) (normal (tvec 0 0)))
  (let ((hit (load-time-value (%make-hit NIL (vec 0 0)))))
    (setf (hit-object hit) object)
    (setf (hit-location hit) location)
    (setf (hit-time hit) time)
    (setf (hit-normal hit) normal)
    hit))

(defun transfer-hit (target source)
  (setf (hit-object target) (hit-object source))
  (setf (hit-location target) (hit-location source))
  (setf (hit-time target) (hit-time source))
  (setf (hit-normal target) (hit-normal source))
  target)

(defclass collider () ())

(defmethod (setf location) :after (loc (collider collider))
  (when (slot-boundp collider 'container)
    (bvh:bvh-update (bvh +world+) collider)))

(defmethod (setf bsize) :after (loc (collider collider))
  (when (slot-boundp collider 'container)
    (bvh:bvh-update (bvh +world+) collider)))

(defclass base-entity (entity)
  ((name :initarg :name :initform NIL :type symbol)))

(defclass located-entity (base-entity transformed)
  ((location :initarg :location :initform (vec 0 0) :accessor location :type vec2)))

(defmethod print-object ((entity located-entity) stream)
  (print-unreadable-object (entity stream :type T :identity T)
    (format stream "~@[~a ~]~a" (name entity) (location entity))))

(defmethod apply-transforms progn ((obj located-entity))
  (translate-by (round (vx (location obj))) (round (vy (location obj))) 0))

(defclass facing-entity (base-entity transformed)
  ((direction :initarg :direction :initform (vec 0 1) :accessor direction :type vec2)))

(defmethod apply-transforms progn ((obj facing-entity))
  (scale-by (float-sign (vx (direction obj))) 1 1))

(defclass rotated-entity (base-entity transformed)
  ((angle :initarg :angle :initform 0f0 :accessor angle :type single-float)))

(defmethod apply-transforms progn ((obj rotated-entity))
  (let ((angle (angle obj)))
    (when (/= 0.0 angle)
      (rotate #.(vec 0 0 1) angle))))

(defclass sized-entity (located-entity)
  ((bsize :initarg :bsize :initform (vec 16 16) :accessor bsize :type vec2)))

(defmethod size ((entity sized-entity))
  (v* (bsize entity) 2))

(defmethod resize ((entity sized-entity) width height)
  (vsetf (bsize entity) (/ width 2) (/ height 2)))

(defmethod scan ((entity sized-entity) (target vec2) on-hit)
  (let ((w (vx2 (bsize entity)))
        (h (vy2 (bsize entity)))
        (loc (location entity)))
    (when (and (<= (- (vx2 loc) w) (vx2 target) (+ (vx2 loc) w))
               (<= (- (vy2 loc) h) (vy2 target) (+ (vy2 loc) h)))
      (let ((hit (make-hit entity (location entity))))
        (unless (funcall on-hit hit) hit)))))

(defmethod scan ((entity sized-entity) (target vec4) on-hit)
  (let ((bsize (bsize entity))
        (loc (location entity)))
    (when (and (< (abs (- (vx2 loc) (vx4 target))) (+ (vx2 bsize) (vz4 target)))
               (< (abs (- (vy2 loc) (vy4 target))) (+ (vy2 bsize) (vw4 target))))
      (let ((hit (make-hit entity (location entity))))
        (unless (funcall on-hit hit) hit)))))

(defmethod scan ((entity sized-entity) (target sized-entity) on-hit)
  (let ((vec (load-time-value (vec4 0 0 0 0)))
        (loc (location target))
        (bsize (bsize target)))
    (vsetf vec (vx2 loc) (vy2 loc) (vx2 bsize) (vy2 bsize))
    (scan entity vec on-hit)))

(defmethod scan-collision (target (entity sized-entity))
  (let ((best-hit (load-time-value (%make-hit NIL (vec 0 0))))
        (best-dist float-features:single-float-positive-infinity))
    (setf (hit-time best-hit) float-features:single-float-positive-infinity)
    (flet ((on-find (hit)
             (when (collides-p entity (hit-object hit) hit)
               (let ((dist (vsqrdist2 (hit-location hit) (location entity))))
                 (when (or (< (hit-time hit) (hit-time best-hit))
                           (and (= (hit-time hit) (hit-time best-hit))
                                (< dist best-dist)))
                   (transfer-hit best-hit hit)
                   (setf best-dist dist))))
             T))
      (scan target entity #'on-find)
      (when (/= (hit-time best-hit) float-features:single-float-positive-infinity)
        best-hit))))

(define-shader-entity animated-sprite (trial:animated-sprite facing-entity sized-entity)
  ())

(define-shader-entity game-entity (animated-sprite collider)
  ((velocity :initarg :velocity :initform (vec2 0 0) :accessor velocity
             :type vec2 :documentation "The velocity of the entity.")
   (state :initform :normal :accessor state
          :type symbol :documentation "The current state of the entity.")
   (frame-velocity :initform (vec2 0 0) :accessor frame-velocity)))

(defmethod (setf location) (location (entity game-entity))
  (vsetf (location entity) (vx location) (vy location)))

(defmethod handle :after ((ev tick) (entity game-entity))
  (let ((vel (frame-velocity entity))
        (bsize (bsize (unit 'farm +world+))))
    (nv+ (location entity) vel)
    (vsetf vel 0 0)
    ;; FIXME: this sucks.
    (setf (vx (location entity)) (clamp (- 100 (vx bsize)) (vx (location entity)) (- (vx bsize) 100)))
    (setf (vy (location entity)) (clamp (- 10 (vy bsize)) (vy (location entity)) (- (vy bsize) 200)))
    (bvh:bvh-update (bvh +world+) entity)))
