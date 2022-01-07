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
                                  :speech 1.0
                                  :music 1.0))
                 :display (:resolution (1280 720)
                           :fullscreen NIL
                           :vsync T
                           :gamma 2.2
                           :ui-scale 1.0
                           :font "PromptFont")
                 :language :eng)))

(defmethod unit (thing (target (eql T)))
  (when +world+
    (unit thing +world+)))

(defun random* (x var)
  (if (= 0.0 var)
      x
      (+ x (- (random var) (/ var 2f0)))))

(defun mouse-world-pos (pos)
  (let ((camera (unit :camera T)))
    (let ((pos (nv+ (v/ pos (view-scale camera) (zoom camera)) (location camera))))
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
