(in-package #:org.shirakumo.fraf.vpetjam)

(deploy:remove-hook :deploy 'org.shirakumo.fraf.trial.alloy::alloy)

(defclass main (org.shirakumo.fraf.trial.notify:main)
  ((scene :initform (make-instance 'world))
   (loader :initform (make-instance 'loader))
   (game-speed :initform 1.0 :accessor game-speed))
  (:default-initargs
   :clear-color (vec 2/17 2/17 2/17 0)
   :context '(:version (3 3) :profile :core)))

(defmethod initialize-instance ((main main) &key audio-backend)
  (setf +main+ main)
  (call-next-method)
  (setf +input-source+ :keyboard)
  (flet ((start (drain)
           (harmony:start (harmony:make-simple-server :name +app-system+ :latency (setting :audio :latency)
                                                      :mixers '(:music (:effect mixed:plane-mixer))
                                                      :effects '((mixed:biquad-filter :filter :lowpass :name :lowpass))
                                                      :drain drain))))
    (handler-case (with-error-logging (:vpetjam "Failed to set up sound, falling back to dummy output.")
                    (start (or audio-backend (setting :audio :backend) :default)))
      (error () (start :dummy))))
  (setf (mixed:min-distance harmony:*server*) 64)
  (setf (mixed:max-distance harmony:*server*) 1024)
  (loop for (k v) on (setting :audio :volume) by #'cddr
        do (setf (harmony:volume k) v)))

(defmethod update ((main main) tt dt fc)
  (let* ((scene (scene main))
         (dt (* (game-speed main) (float dt 1.0)))
         (ev (load-time-value (make-instance 'tick))))
    (setf (slot-value ev 'tt) tt)
    (setf (slot-value ev 'dt) dt)
    (setf (slot-value ev 'fc) fc)
    (issue scene ev)
    (process scene)))

(defmethod (setf scene) :after (scene (main main))
  (setf +world+ scene))

(defmethod finalize :after ((main main))
  (when harmony:*server*
    (harmony:free harmony:*server*))
  (setf harmony:*server* NIL)
  (setf +world+ NIL)
  (setf +main+ NIL))

(defun main ()
  (launch))

(defmethod render-loop :around ((main main))
  (let ((*package* #.*package*))
    (call-next-method)))

(defun launch (&rest initargs)
  (let ((*package* #.*package*))
    (load-keymap)
    (ignore-errors
     (load-settings))
    (save-settings)
    (apply #'trial:launch 'main
           :context (list :width (first (setting :display :resolution))
                          :height (second (setting :display :resolution))
                          :vsync (setting :display :vsync)
                          :fullscreen (setting :display :fullscreen)
                          :title +app-system+
                          :version '(3 3)
                          :profile :core)
           (append (setting :debugging :initargs) initargs))))

(defmethod setup-scene ((main main) (scene scene))
  (enter (make-instance 'camera) scene)
  (let ((render (make-instance 'render-pass))
        (ui (make-instance 'ui-pass :base-scale (setting :display :ui-scale)))
        (blend (make-instance 'blend-pass)))
    (connect (port render 'color) (port blend 'trial::a-pass) scene)
    (connect (port ui 'color) (port blend 'trial::b-pass) scene))
  scene)

(defmethod setup-rendering :after ((main main))
  (disable :cull-face :scissor-test :depth-test))

(defun apply-video-settings (&optional (settings (setting :display)))
  (when *context*
    (destructuring-bind (&key resolution fullscreen vsync ui-scale gamma &allow-other-keys) settings
      (when (and gamma (unit 'render (scene +main+)))
        (setf (monitor-gamma (unit 'render (scene +main+))) gamma))
      (show *context* :fullscreen fullscreen :mode resolution)
      (setf (vsync *context*) vsync)
      (setf (alloy:base-scale (unit 'ui-pass T)) ui-scale))))

(define-setting-observer volumes :audio :volume (value)
  (when harmony:*server*
    (loop for (k v) on value by #'cddr
          do (setf (harmony:volume k) v))))

(define-setting-observer video :display (value)
  (apply-video-settings value))
