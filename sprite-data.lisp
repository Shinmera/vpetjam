(in-package #:org.shirakumo.fraf.vpetjam)

(defun read-src (file)
  (with-trial-io-syntax ()
    (with-open-file (stream file :direction :input
                                 :element-type 'character)
      (read stream))))

(defun aseprite (&rest args)
  (uiop:run-program (list* #-windows "aseprite" #+windows "aseprite.exe"
                           "-b"
                           (loop for arg in args
                                 collect (typecase arg
                                           (pathname (uiop:native-namestring arg))
                                           (T (princ-to-string arg)))))
                    :output *standard-output*
                    :error-output *error-output*))

(defclass sprite-data (compiled-generator trial:sprite-data)
  ((json-file :initform NIL :accessor json-file)
   (source :initform NIL :accessor source)
   (palette :initform NIL :accessor palette)
   (palettes :initform NIL :accessor palettes)))

(defmethod notify:files-to-watch append ((asset sprite-data))
  (list (merge-pathnames (getf (read-src (input* asset)) :source) (input* asset))))

(defmethod notify:notify :before ((asset sprite-data) file)
  (when (string= "ase" (pathname-type file))
    (sleep 1)
    (compile-resources asset T)))

(defmethod write-animation ((sprite sprite-data) &optional (stream T))
  (let ((*package* #.*package*))
    (format stream "(:source ~s~%" (source sprite))
    (format stream " :animation-data ~s~%" (json-file sprite))
    (format stream " :palette ~s~%" (palette sprite))
    (format stream " :palettes ~s~%" (palettes sprite))
    (format stream " :animations~%  (")
    (loop for animation across (animations sprite)
          do (write-animation animation stream))
    (format stream "))~%")))

(defmethod write-animation ((animation sprite-animation) &optional (stream T))
  (format stream "~&   (~20a :start ~3d :end ~3d :loop-to ~3a :next ~s)"
          (name animation)
          (start animation)
          (end animation)
          (loop-to animation)
          (etypecase (next-animation animation)
            (symbol (next-animation animation))
            (trial:sprite-animation (name (next-animation animation))))))

(defmethod compile-resources ((sprite sprite-data) (path pathname) &key force)
  (destructuring-bind (&key source palette (albedo (make-pathname :type "png" :defaults source))
                            (animation-data (make-pathname :type "json" :defaults source)) &allow-other-keys) (read-src path)
    (let ((source (merge-pathnames source path))
          (animation-data (merge-pathnames animation-data path))
          (albedo (merge-pathnames albedo path)))
      (when (or force (recompile-needed-p (list albedo animation-data)
                                          (list source path)))
        (v:info :kandria.resources "Compiling spritesheet from ~a..." source)
        (aseprite "--sheet-pack"
                  "--trim"
                  "--shape-padding" "1"
                  "--sheet" albedo
                  "--format" "json-array"
                  "--filename-format" "{tagframe} {tag}"
                  "--list-tags"
                  "--data" animation-data
                  source)
        ;; Convert palette colours
        #++
        (when palette
          (convert-palette albedo (merge-pathnames palette path)))))))

(defmethod generate-resources ((sprite sprite-data) (path pathname) &key)
  (destructuring-bind (&key source palette palettes (animation-data (make-pathname :type "json" :defaults source)) animations frames &allow-other-keys) (read-src path)
    (setf (json-file sprite) animation-data)
    (setf (source sprite) source)
    (setf (palette sprite) palette)
    (setf (palettes sprite) palettes)
    (prog1 (with-trial-io-syntax ()
             (call-next-method sprite (merge-pathnames animation-data path)))
      (loop for expr in animations
            do (destructuring-bind (name &key start end loop-to next) expr
                 (let ((animation (find name (animations sprite) :key #'name)))
                   (when animation
                     (change-class animation 'sprite-animation
                                   :loop-to loop-to
                                   :next-animation next)
                     ;; Attempt to account for changes in the frame counts of the animations
                     ;; by updating frame data per-animation here. We have to assume that
                     ;; frames are only removed or added at the end of an animation, as we
                     ;; can't know anything more.
                     (when (and start end (< 0 (length frames)))
                       (let ((rstart (start animation)))
                         (when (< (loop-to animation) rstart)
                           (setf (loop-to animation) (+ rstart (- (loop-to animation) start))))))))))
      ;; Make sure all animations are in the correct class.
      (loop for animation across (animations sprite)
            do (unless (typep animation 'sprite-animation) (change-class animation 'sprite-animation))))))
