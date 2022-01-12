(in-package #:org.shirakumo.fraf.vpetjam)

(defparameter *genes* (make-hash-table :test 'equal))

(defun genes-of (genome)
  (declare (type keyword genome))
  (loop for key in (alexandria:hash-table-keys *genes*)
        when (eql genome (car key)) collect (gethash key *genes*)))

(defun make-gene (name genome value &optional recessive)
  (let ((gene (make-instance 'gene :genome genome :id name :value value :recessive recessive)))
     (setf (gethash (key gene) *genes*) gene)
     gene))

(defun make-hue-gene (name hue &optional recessive)
  (make-gene name :hue hue recessive))

(progn
  (make-hue-gene :red (vec3 1 0 0))
  (make-hue-gene :green (vec3 0 1 0))
  (make-hue-gene :blue (vec3 0 0 1)))
