(in-package #:org.shirakumo.fraf.vpetjam)

(defparameter *genes* (make-hash-table :test 'equal))

(defun gene-of (genome id &key error)
  (or (gethash (cons genome id) *genes*)
      (when error (error "No such gene: ~a,~a" genome id))))

(defun genes-of (genome &key error)
  (declare (type keyword genome))
  (or (loop for key in (alexandria:hash-table-keys *genes*)
            when (eql genome (car key)) collect (gethash key *genes*))
      (when error (error "No such genome: ~a" genome))))

(defun make-gene (genome name value &optional recessive)
  (let ((gene (make-instance 'gene :genome genome :id name :value value :recessive recessive)))
     (setf (gethash (key gene) *genes*) gene)
     gene))

(defun make-unique-gene (genome value &optional recessive)
  (let ((name (intern (format NIL "~a-~d" genome (1+ (length (genes-of genome)))) :keyword)))
    (make-gene genome name value recessive)))

(progn
  (loop for key in (alexandria:hash-table-keys *genes*)
        do (remhash key *genes*))

  (make-unique-gene :hue 0.0)
  (make-unique-gene :hue 1.5)
  (make-unique-gene :hue 3.0)
  (make-unique-gene :hue 4.5)
  (make-unique-gene :hue 6.0)

  (make-gene :speed :extra-slow 0.2)
  (make-gene :speed :very-slow 0.5)
  (make-gene :speed :slow 1.0)
  (make-gene :speed :medium 3.0)
  (make-gene :speed :fast 5.0)
  (make-gene :speed :extra-fast 10.0)

  (dotimes (i 5)
    (make-unique-gene :body i)
    (make-unique-gene :face i)))
