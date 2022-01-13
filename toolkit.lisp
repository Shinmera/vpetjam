(in-package #:org.shirakumo.fraf.vpetjam)

(define-global +world+ NIL)
(define-global +main+ NIL)
(define-global +input-source+ :keyboard)
(define-global +app-system+ "vpetjam")
(define-global +settings+
    (copy-tree '(:audio (:latency 0.05
                         :backend :default
                         :volume (:master 0.5
                                  :effect 1.0
                                  :music 1.0))
                 :display (:resolution (1280 720)
                           :fullscreen NIL
                           :vsync T
                           :gamma 2.2
                           :ui-scale 1.0
                           :font "PromptFont")
                 :language :eng)))

(declaim (inline v<-))
(defun v<- (target source)
  (etypecase source
    (vec2 (vsetf target (vx2 source) (vy2 source)))
    (vec3 (vsetf target (vx3 source) (vy3 source) (vz3 source)))
    (vec4 (vsetf target (vx4 source) (vy4 source) (vz4 source) (vw4 source)))))

(defmacro tvec (&rest args)
  `(vsetf (load-time-value (vec ,@(loop repeat (length args) collect 0)))
          ,@args))

(defun nvalign (vec grid)
  (flet ((frob (x)
           (* (round x grid) grid)))
    (vapplyf vec frob)))

(defun vsqrdist2 (a b)
  (declare (optimize speed))
  (+ (expt (- (vx a) (vx b)) 2)
     (expt (- (vy a) (vy b)) 2)))

(defmethod unit (thing (target (eql T)))
  (when +world+
    (unit thing +world+)))

(defun random* (x var)
  (if (= 0.0 var)
      x
      (+ x (- (random var) (/ var 2f0)))))

(defun mouse-world-pos (pos)
  (let ((camera (unit :camera T)))
    (let ((pos (nv+ (v/ pos (view-scale camera) (zoom camera))
                    (vxy (location camera)))))
      (nv- pos (v/ (target-size camera) (zoom camera))))))

(defun world-screen-pos (pos)
  (let ((camera (unit :camera T)))
    (let ((pos (v+ pos (v/ (target-size camera) (zoom camera)))))
      (v* (nv- pos (location camera)) (view-scale camera) (zoom camera)))))

(defun generate-name (&optional indicator)
  (loop for name = (format NIL "~a-~d" (or indicator "ENTITY") (incf *gensym-counter*))
        while (find-symbol name #.*package*)
        finally (return (intern name #.*package*))))

(set-dispatch-macro-character
 #\# #\! (lambda (s c a)
           (declare (ignore c a))
           (let ((inner (read s T NIL T)))
             `(when +world+
                (with-eval-in-render-loop (+world+)
                  ,inner)))))

(defun maybe-leave (thing)
  (when (slot-boundp thing 'container)
    (leave* thing T)))
