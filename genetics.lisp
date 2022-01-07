(in-package #:org.shirakumo.fraf.vpetjam)

(defclass gene ()
  ((genome :initarg :genome :accessor genome)
   (name :initarg :name :accessor name)
   (inherit :initarg :inherit :accessor inherit)
   (recessive :initarg :recessive :accessor recessive))
  (:default-initargs :genome (error "GENOME required")
                     :name (error "NAME required")
                     :inherit 1
                     :recessive NIL))

(defclass genetical ()
  ((genes :initform (make-hash-table) :reader genes)
   (parents :initarg :parents :initform NIL :accessor parents)))

(defmethod genomes ((critter genetical) &key ancestors)
  (let ((genomes (loop for genome in (alexandria:hash-table-keys (genes critter))
                       collect genome)))
    (when ancestors
      (loop for parent in (parents critter)
            for inherited = (genomes parent :ancestors T)
            unless (gethash inherited (genes critter))
            do (push inherited genomes)))
    genomes))

(defmethod gene ((critter genetical) genome)
  (declare (type keyword genome))
  (gethash genome critter))

(defmethod set-gene ((critter genetical) (gene gene))
  (setf (gethash (genome gene) (genes critter)) gene))

(defmethod cross ((critter genetical) (other genetical))
  ;; TODO: Skipping generations.
  (flet ((frandom () (coerce (/ (random #x7fffffff) #x7fffffff) 'single-float)))
    (let ((child (make-instance 'genetical :parents (list critter other)))
          (genomes (genomes critter)))
      (loop for gene in (genomes other)
            unless (gethash gene (genes critter))
            do (push gene genomes))
      (loop for genome in genomes
            for gene-a = (gene critter genome)
            for gene-b = (gene other genome)
            for maybe-a = (when (< (frandom) (inherit gene-a)) gene-a)
            for maybe-b = (when (< (frandom) (inherit gene-b)) gene-b)
            for inherited = (cond
                              ((and maybe-a maybe-b (eql (recessive maybe-a) (recessive maybe-b)))
                               (if (< (frandom) 0.5) maybe-a maybe-b))
                              ((and maybe-a maybe-b)
                               (if (< (frandom) 0.375)
                                   (if (recessive maybe-a) maybe-a maybe-b)
                                   (if (recessive maybe-a) maybe-b maybe-a)))
                              (T (or maybe-a maybe-b)))
            when inherited do (set-gene child inherited))
      child)))
