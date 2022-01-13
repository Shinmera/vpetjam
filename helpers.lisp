(in-package #:org.shirakumo.fraf.vpetjam)

(define-asset (vpetjam 1x) mesh
    (make-rectangle 1 1))

(define-asset (vpetjam 64x) mesh
    (make-rectangle 64 64))

(define-asset (vpetjam 128x) mesh
    (make-rectangle 128 128))

(define-asset (vpetjam 512x) mesh
    (make-rectangle 512 512))

(define-asset (vpetjam placeholder) image
    #p"placeholder.png")

(define-shader-entity player () ())

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

(define-shader-entity basic-entity (vertex-entity textured-entity sized-entity)
  ((vertex-array :initform (// 'vpetjam '1x))))

(defmethod apply-transforms progn ((entity basic-entity))
  (scale-by (* 2.0 (vx (bsize entity))) (* 2.0 (vy (bsize entity))) 1.0))

(define-shader-entity animated-sprite (trial:animated-sprite facing-entity sized-entity)
  ())

(defclass game-entity (sized-entity listener collider)
  ((velocity :initarg :velocity :initform (vec2 0 0) :accessor velocity
             :type vec2 :documentation "The velocity of the entity.")
   (state :initform :normal :accessor state
          :type symbol :documentation "The current state of the entity.")
   (frame-velocity :initform (vec2 0 0) :accessor frame-velocity)
   (hit-border :initform NIL :accessor hit-border)))

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
    (cond ((not (<= (- 100 (vx bsize)) (vx loc) (- (vx bsize) 100)))
           (setf (hit-border entity) :x))
          ((not (<= (- 100 (vy bsize)) (vy loc) (- (vy bsize) 200)))
           (setf (hit-border entity) :y))
          (T
           (setf (hit-border entity) NIL)))
    (when (hit-border entity)
      (setf (vx loc) (clamp (- 100 (vx bsize)) (vx loc) (- (vx bsize) 100)))
      (setf (vy loc) (clamp (- 100 (vy bsize)) (vy loc) (- (vy bsize) 200))))
    (setf (vz loc) (vy loc))
    (bvh:bvh-update (bvh +world+) entity)))

(define-shader-entity part (vertex-entity textured-entity rotated-entity located-entity)
  ((vertex-array :initform (// 'vpetjam '64x))
   (texture :initform (// 'vpetjam 'player))
   (uv-offset :initarg :uv-offset :initform (vec 0 0) :accessor uv-offset)
   (hue :initform 0.0 :accessor hue))
  (:inhibit-shaders (textured-entity :vertex-shader)))

(defmethod render :before ((part part) (program shader-program))
  (setf (uniform program "hue") (float (hue part) 0f0))
  (setf (uniform program "uv_offset") (uv-offset part)))

(defmethod render-child ((part part) (program shader-program))
  (with-pushed-matrix ()
    (apply-transforms part)
    (gl:bind-texture :texture-2d (gl-name (texture part)))
    (setf (uniform program "hue") (float (hue part) 0f0))
    (setf (uniform program "uv_offset") (uv-offset part))
    (setf (uniform program "model_matrix") (model-matrix))
    (%gl:draw-elements :triangles 6 :unsigned-int 0)))

(defmethod (setf direction) (dir (part part)))

(define-class-shader (part :vertex-shader)
  "layout (location = 1) in vec2 in_texcoord;
uniform vec2 uv_offset = vec2(0,0);
uniform sampler2D tex_image;
out vec2 texcoord;

void main(){
  ivec2 size = textureSize(tex_image, 0);
  texcoord = (uv_offset+(in_texcoord*0.999))*(64.0/size.x);
}")

(define-class-shader (part :fragment-shader)
  "uniform float hue = 0.0;
out vec4 color;

void main(){
  const vec3  kRGBToYPrime = vec3 (0.299, 0.587, 0.114);
  const vec3  kRGBToI      = vec3 (0.596, -0.275, -0.321);
  const vec3  kRGBToQ      = vec3 (0.212, -0.523, 0.311);

  const vec3  kYIQToR     = vec3 (1.0, 0.956, 0.621);
  const vec3  kYIQToG     = vec3 (1.0, -0.272, -0.647);
  const vec3  kYIQToB     = vec3 (1.0, -1.107, 1.704);

  float   YPrime  = dot (color.rgb, kRGBToYPrime);
  float   I       = dot (color.rgb, kRGBToI);
  float   Q       = dot (color.rgb, kRGBToQ);
  float   h       = atan (Q, I);
  float   chroma  = sqrt (I * I + Q * Q);

  h += hue;

  Q = chroma * sin (h);
  I = chroma * cos (h);

  vec3    yIQ   = vec3 (YPrime, I, Q);

  color.rgb = vec3( dot (yIQ, kYIQToR), dot (yIQ, kYIQToG), dot (yIQ, kYIQToB) );
}")

(define-shader-entity part-parent (part)
  ((children :initform (make-hash-table :test 'eql) :accessor children)
   (render-list :initform () :accessor render-list)))

(defmethod shared-initialize :after ((entity part-parent) slots &key (children () children-p))
  (when children-p
    (setf (children entity) children)))

(defmethod (setf children) ((children cons) (entity part-parent))
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
        do (setf (gethash name (children entity)) part))
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
      (setf (uniform program "hue") (float (hue entity) 0f0))
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

(defclass interactable (sized-entity)
  ())

(defgeneric interact (interactable source))
