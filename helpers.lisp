(in-package #:org.shirakumo.fraf.vpetjam)

(define-asset (vpetjam 1x) mesh
    (make-rectangle 1 1 :align :bottomleft))

(define-asset (vpetjam 64x) mesh
    (make-rectangle 64 64))

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

(defclass facing-entity (base-entity transformed)
  ((direction :initarg :direction :initform (vec 0 -1) :accessor direction :type vec2)))

(defmethod apply-transforms progn ((obj facing-entity))
  (scale-by (float-sign (vx (direction obj))) 1 1))

(defclass rotated-entity (base-entity transformed)
  ((angle :initarg :angle :initform 0f0 :accessor angle :type single-float)
   (pivot :initarg :pivot :initform (vec 0 0) :accessor pivot :type vec2)
   (skew :initarg :skew :initform (vec 1 1) :accessor skew)))

(defmethod apply-transforms progn ((obj rotated-entity))
  (let ((angle (angle obj)))
    (translate-by (vx (pivot obj)) (vy (pivot obj)) 0)
    (scale-by (vx (skew obj)) (vy (skew obj)) 1)
    (rotate #.(vec 0 0 1) angle)
    (translate-by (- (vx (pivot obj))) (- (vy (pivot obj))) 0)))

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

(defclass game-entity (sized-entity listener collider)
  ((velocity :initarg :velocity :initform (vec2 0 0) :accessor velocity
             :type vec2 :documentation "The velocity of the entity.")
   (state :initform :normal :accessor state
          :type symbol :documentation "The current state of the entity.")
   (frame-velocity :initform (vec2 0 0) :accessor frame-velocity)))

(defmethod (setf location) (location (entity game-entity))
  (vsetf (location entity) (vx location) (vy location)))

(defmethod handle :after ((ev tick) (entity game-entity))
  (let ((vel (frame-velocity entity))
        (loc (location entity))
        (bsize (bsize (unit 'farm +world+))))
    (incf (vx loc) (vx vel))
    (incf (vy loc) (vy vel))
    (vsetf vel 0 0)
    ;; FIXME: this sucks.
    (setf (vx loc) (clamp (- 100 (vx bsize)) (vx loc) (- (vx bsize) 100)))
    (setf (vy loc) (clamp (- 10 (vy bsize)) (vy loc) (- (vy bsize) 200)))
    (setf (vz loc) (vy loc))
    (bvh:bvh-update (bvh +world+) entity)))

(define-shader-entity part (vertex-entity textured-entity rotated-entity located-entity)
  ((vertex-array :initform (// 'vpetjam '64x))
   (texture :initform (// 'vpetjam 'player))
   (uv-offset :initarg :uv-offset :initform (vec 0 0) :accessor uv-offset))
  (:inhibit-shaders (textured-entity :vertex-shader)))

(defmethod render :before ((part part) (program shader-program))
  (setf (uniform program "uv_offset") (uv-offset part)))

(defmethod render-child ((part part) (program shader-program))
  (with-pushed-matrix ()
    (apply-transforms part)
    (setf (uniform program "uv_offset") (uv-offset part))
    (setf (uniform program "model_matrix") (model-matrix))
    (%gl:draw-elements :triangles 6 :unsigned-int 0)))

(define-class-shader (part :vertex-shader)
  "layout (location = 1) in vec2 in_texcoord;
uniform vec2 uv_offset = vec2(0,0);
out vec2 texcoord;

void main(){
  texcoord = (uv_offset+(in_texcoord*0.999))*(64.0/512.0);
}")

(define-shader-entity part-parent (part)
  ((children :initform (make-hash-table :test 'eql) :accessor children)
   (render-list :initform () :accessor render-list)))

(defmethod shared-initialize :after ((entity part-parent) slots &key (children () children-p))
  (when children-p
    (clrhash (children entity))
    (loop for (name . props) in children
          for part = (destructuring-bind (&key uv (pivot '(0 0)) (location '(0 0 0)) (skew '(1 1)) (angle 0) (children ())) props
                       (flet ((vecify (parts)
                                (apply #'vec parts)))
                         (make-instance 'part-parent
                                        :name name
                                        :texture (texture entity)
                                        :uv-offset (vecify uv)
                                        :pivot (vecify pivot)
                                        :skew (vecify skew)
                                        :angle angle
                                        :location (vecify location)
                                        :children children)))
          do (setf (gethash name (children entity)) part)))
  (setf (render-list entity) (sort (alexandria:hash-table-values (children entity)) #'>
                                   :key (lambda (p) (vz (location p))))))

(defmethod part (name (entity part-parent))
  (gethash name (children entity)))

(defmethod (setf part) ((part part) name (entity part-parent))
  (setf (gethash name (children entity)) part)
  (setf (render-list entity) (sort (alexandria:hash-table-values (children entity)) #'>
                                   :key (lambda (p) (vz (location p))))))

(defmethod render ((entity part-parent) (program shader-program))
  (setf (uniform program "view_matrix") (view-matrix))
  (setf (uniform program "projection_matrix") (projection-matrix))
  (let* ((vao (vertex-array entity))
         (children (render-list entity)))
    (gl:bind-vertex-array (gl-name vao))
    (with-pushed-matrix ((model-matrix :identity))
      (render-child entity program))))

(defmethod render-child ((entity part-parent) (program shader-program))
  (with-pushed-matrix ()
    (apply-transforms entity)
    (let ((children (render-list entity)))
      (loop for child = (car children)
            while (and child (< 0 (vz (location child))))
            do (pop children)
               (render-child child program))
      (gl:bind-texture :texture-2d (gl-name (texture entity)))
      (setf (uniform program "uv_offset") (uv-offset entity))
      (setf (uniform program "model_matrix") (model-matrix))
      (%gl:draw-elements :triangles 6 :unsigned-int 0)
      (loop for child = (pop children)
            while child
            do (render-child child program)))))

(defmethod (setf direction) (dir (entity part-parent))
  (let ((off (cond ((> 0 (vy dir)) 0)
                   ((= 0 (vy dir)) 1)
                   ((< 0 (vy dir)) 2))))
    (setf (vx (uv-offset entity)) off)
    (dolist (child (render-list entity))
      (if (typep child 'part-parent)
          (setf (direction child) dir)
          (setf (vx (uv-offset child)) off)))))
