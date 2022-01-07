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

(defmethod handle ((ev mouse-press) (world world))
  (when (eql :middle (button ev))
    (enter-and-load (make-instance 'crop :location (vxyy (mouse-world-pos (pos ev)))) world +main+)))

(defun queue-sort (queue comparator)
  (let ((elements (make-array (flare-queue:queue-size queue))))
    (loop for cell = (flare-queue:right (flare-queue::head queue)) then (flare-queue:right cell)
          for i from 0
          until (eq cell (flare-queue::tail queue))
          do (setf (aref elements i) cell))
    (sort elements (lambda (a b)
                     (funcall comparator (flare-queue:value a) (flare-queue:value b))))
    (loop for left = (flare-queue::head queue) then right
          for right across elements
          do (flare-queue:cell-tie left right))
    (flare-queue:cell-tie (aref elements (1- (length elements)))
                          (flare-queue::tail queue))))

(defmethod process :after ((world world))
  ;; z-sort. Really terribly inefficient.
  (queue-sort (objects world)
              (lambda (a b)
                (cond ((and (typep a 'located-entity) (typep b 'located-entity))
                       (< (vz (location b)) (vz (location a))))
                      ((typep a 'located-entity)
                       NIL)
                      ((typep b 'located-entity)
                       T)
                      (T
                       T))))
  (compile-to-pass world world))

