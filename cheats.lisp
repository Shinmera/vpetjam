(in-package #:org.shirakumo.fraf.vpetjam)

(defvar *cheat-codes* ())

(defstruct (cheat (:constructor make-cheat (name code effect)))
  (name NIL :type symbol)
  (idx 0 :type (unsigned-byte 8))
  (code "" :type simple-base-string)
  (effect NIL :type function))

(defun cheat (name)
  (find name *cheat-codes* :key #'cheat-name))

(defun (setf cheat) (cheat name)
  (let ((cheats (remove name *cheat-codes* :key #'cheat-name)))
    (setf *cheat-codes* (if cheat (list* cheat cheats) cheats))
    cheat))

(defmacro define-cheat (code &body action)
  (destructuring-bind (name code) (enlist code code)
    `(setf (cheat ',name) (make-cheat ',name
                                      ,(string-downcase code)
                                      (lambda () ,@action)))))

(defun process-cheats (key)
  (loop for cheat in *cheat-codes*
        for i = (cheat-idx cheat)
        for code = (cheat-code cheat)
        do (let ((new (if (string= key code :start2 i :end2 (+ i (length key))) (1+ i) 0)))
             (cond ((<= (length code) new)
                    (setf (cheat-idx cheat) 0)
                    (v:info :kandria.cheats "Activating cheat code ~s" (cheat-name cheat)))
                   (T
                    (setf (cheat-idx cheat) new))))))
