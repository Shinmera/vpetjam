(in-package #:org.shirakumo.fraf.vpetjam)

(defclass world (pipelined-scene)
  ((bvh :initform (bvh:make-bvh) :reader bvh)))

(defmethod clear :after ((world world))
  (clear (bvh world)))

(defmethod enter :after ((unit collider) (world world))
  (bvh:bvh-insert (bvh world) unit))

(defmethod leave :after ((unit collider) (world world))
  (bvh:bvh-remove (bvh world) unit))

(defmethod scan ((world world) target on-hit)
  (bvh:do-fitting (object (bvh world) target)
    (unless (eq object target)
      (let ((hit (scan object target on-hit)))
        (when hit
          (return hit))))))

(defmethod scan ((world world) (target game-entity) on-hit)
  (let ((loc (location target))
        (bsize (bsize target))
        (vel (frame-velocity target)))
    (bvh:do-fitting (object (bvh world) (tvec (- (vx2 loc) (vx2 bsize) 20)
                                               (- (vy2 loc) (vy2 bsize) 20)
                                               (+ (vx2 loc) (vx2 bsize) 20)
                                               (+ (vy2 loc) (vy2 bsize) 20)))
      (unless (eq object target)
        (let ((hit (scan object target on-hit)))
          (when hit (return hit)))))))

(defmethod scan ((world world) (target vec4) on-hit)
  (bvh:do-fitting (object (bvh world) (tvec (- (vx4 target) (vz4 target))
                                             (- (vy4 target) (vw4 target))
                                             (+ (vx4 target) (vz4 target))
                                             (+ (vy4 target) (vw4 target))))
    (let ((hit (scan object target on-hit)))
      (when hit (return hit)))))

(progn
  (defmethod setup-scene ((main trial:main) (world world))
    (enter (make-instance 'farm) world)
    (enter (make-instance 'player) world)
    (enter (make-instance 'camera) world)
    #-vpetjam-release
    (enter (make-instance 'trial::fps-counter) (scene main)))
  #!(issue +world+ 'reload-scene))
