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

(defun make-gene (name genome value &optional recessive)
  (let ((gene (make-instance 'gene :genome genome :id name :value value :recessive recessive)))
     (setf (gethash (key gene) *genes*) gene)
     gene))

(defun make-hue-gene (name hue &optional recessive)
  (make-gene name :hue hue recessive))

(progn
  (make-hue-gene :white (vec3 1 1 1))
  (make-hue-gene :red (vec3 1 0 0))
  (make-hue-gene :green (vec3 0 1 0))
  (make-hue-gene :blue (vec3 0 0 1)))
