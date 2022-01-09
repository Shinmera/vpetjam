(in-package #:org.shirakumo.fraf.vpetjam)

(defvar *clips* (make-hash-table :test 'eq))

(defmethod clip ((name symbol))
  (gethash name *clips*))

(defmethod (setf clip) (clip (name symbol))
  (setf (gethash name *clips*) clip))

(defmacro define-clip (name &body timeline)
  (let ((timeline `(list ,@(loop for (tt fields) on timeline by #'cddr
                                 for parsed-fields = (loop for field in fields
                                                           collect (destructuring-bind (field value) field
                                                                     `(list ',field ,value)))
                                 collect `(list ,tt ,@parsed-fields)))))
    `(let ((clip (or (clip ',name)
                     (setf (clip ',name) (make-instance 'clip)))))
       (reinitialize-instance clip :timeline ,timeline))))

(defclass clip ()
  ((stops :accessor stops)
   (fields :accessor fields)
   (vals :accessor vals)))

(defmethod shared-initialize :after ((clip clip) slots &key timeline)
  (when timeline
    (let* ((length (max 2 (length timeline)))
           (stops (make-array length :element-type 'single-float :initial-element most-positive-single-float))
           (fields ())
           vals)
      (flet ((ensure-value (value)
               (typecase value
                 (real (float value 0f0))
                 (T value))))
        (loop for (tt . values) in timeline
              for i from 0
              do (setf (aref stops i) (float tt 0f0))
                 (loop for value in values
                       do (pushnew (first value) fields :test #'equal)))
        ;; Set keyframes
        (setf vals (make-array (length fields)))
        (loop for field in fields
              for i from 0
              for values = (setf (aref vals i) (make-array length :initial-element NIL))
              do (loop for frame in timeline
                       for j from 0
                       for value = (ensure-value (second (assoc field (rest frame) :test #'equal)))
                       do (setf (aref values j) value)))
        ;; Clamp ends
        (loop for values across vals
              for first = (position NIL values :test-not #'eq)
              for last = (position NIL values :test-not #'eq :from-end T)
              do (loop for i from 0 below first
                       do (setf (aref values i) (aref values first)))
                 (loop for i from (1+ last) below length
                       do (setf (aref values i) (aref values last))))
        ;; Interpolate keyframes
        (loop for values across vals
              do (loop for i from 0 below length
                       do (when (null (aref values i))
                            (let* ((left-idx (1- i))
                                   (right-idx (position NIL values :test-not #'eq :start i))
                                   (duration (- (aref stops right-idx) (aref stops left-idx)))
                                   (x (/ (- (aref stops i) (aref stops left-idx)) duration)))
                              (setf (aref values i) (mix (aref values left-idx) (aref values right-idx) x))))))
        (setf (stops clip) stops)
        (setf (fields clip) (coerce fields 'vector))
        (setf (vals clip) vals)))))

(defmethod duration ((clip clip))
  (aref (stops clip) (1- (length (stops clip)))))

(defmethod mix ((a real) (b real) x)
  (let ((a (float a 0f0))
        (b (float b 0f0)))
    (+ a (* (- b a) x))))

(defmethod mix ((a float) (b float) x)
  (+ a (* (- b a) x)))

(defmethod mix ((a vec2) (b vec2) x)
  (vlerp a b x))

(defmethod mix ((a vec3) (b vec3) x)
  (vlerp a b x))

(define-clip foo
  0 ((position (vec 0 0)) (angle 0.0))
  1 ((angle 2.0) (skew 1.0))
  1.5 ((skew 0.5))
  2 ((angle 0.0) (skew 1.0)))

(defclass playhead ()
  ((clip :initarg :clip :initform NIL :accessor clip)
   (state :initarg :state :initform :stopped :accessor state)
   (clock :initform 0.0 :accessor clock)
   (index :initform 0 :accessor index)
   (loop-p :initarg :loop :initform NIL :accessor loop-p)))

(defgeneric field (field target))
(defgeneric (setf field) (value field target))
(defgeneric advance (playhead target dt))

(defmethod handle :after ((ev tick) (playhead playhead))
  (advance playhead playhead (dt ev)))

(defmethod advance ((playhead playhead) target dt)
  (let ((clock (+ (clock playhead) dt))
        (index (index playhead))
        (clip (clip playhead)))
    (when clip
      (when (<= clock 0.0)
        (loop for i from 0 below (length (fields clip))
              for field = (aref (fields clip) i)
              for vals = (aref (vals clip) i)
              do (setf (field field target) (mix (field field target) (aref vals 0) 0.3)))
        (setf (clock playhead) clock)
        (return-from advance))
      (when (<= (duration clip) clock)
        (cond ((loop-p playhead)
               (setf clock (mod clock (duration clip)))
               (setf index 0))
              (T
               (setf clock (duration clip))
               (setf index (- (length (stops clip)) 2))
               (setf (state playhead) :stopped))))
      (loop while (< (aref (stops clip) (1+ index)) clock)
            do (incf index))
      (cond ((= (aref (stops clip) index) clock)
             (loop for field across (fields clip)
                   for value across (aref (vals clip) index)
                   do (setf (field field target) value)))
            (T
             (loop with x = (/ (- clock (aref (stops clip) index))
                               (- (aref (stops clip) (1+ index)) (aref (stops clip) index)))
                   for i from 0 below (length (fields clip))
                   for field = (aref (fields clip) i)
                   for vals = (aref (vals clip) i)
                   for prev = (aref vals index)
                   for next = (aref vals (1+ index))
                   do (setf (field field target) (mix prev next x)))))
      (setf (clock playhead) clock)
      (setf (index playhead) index))))

(defmethod reset ((playhead playhead))
  (setf (clock playhead) -0.1)
  (setf (index playhead) 0)
  playhead)

(defmethod start ((playhead playhead))
  (setf (state playhead) :running)
  playhead)

(defmethod (setf clip) ((clip symbol) (playhead playhead))
  (setf (clip playhead) (or (clip clip)
                            (error "No such clip ~s" clip))))

(defmethod (setf clip) ((clip null) (playhead playhead))
  (setf (slot-value playhead 'clip) NIL))

(defmethod start-clip (clip (playhead playhead))
  (setf (clip playhead) clip)
  (setf (loop-p playhead) NIL)
  (start playhead)
  (reset playhead))

(defmethod start-loop (clip (playhead playhead))
  (start-loop (or (clip clip)
                  (error "No such clip ~s" clip))
              playhead))

(defmethod start-loop ((clip clip) (playhead playhead))
  (unless (eq clip (clip playhead))
    (reset playhead))
  (setf (clip playhead) clip)
  (setf (loop-p playhead) T)
  (start playhead))
