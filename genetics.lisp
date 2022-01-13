(in-package #:org.shirakumo.fraf.vpetjam)

(defclass gene ()
  ((id :initarg :id :reader id)
   (genome :initarg :genome :reader genome)
   (value :initarg :value :accessor value)
   (recessive :initarg :recessive :accessor recessive))
  (:default-initargs :genome (error "GENOME required")
                     :id NIL
                     :value NIL
                     :recessive NIL))

(defmethod key ((gene gene))
  (cons (genome gene) (id gene)))

(defmethod cross ((gene gene) (other gene))
  (unless (eql (genome gene) (genome other))
    (error "May only cross genes of the same genome."))
  (when (or (eql gene other) (eql (id gene) (id other)))
    (return-from cross gene))
  (cond
    ((and gene other (eql (recessive gene) (recessive other)))
     (if (< (random 1.0) 0.5) gene other))
    ((and gene other)
     (if (< (random 1.0) 0.375)
         (if (recessive gene) gene other)
         (if (recessive gene) other gene)))
    (T (or gene other))))

(defclass genetical ()
  ((genes :initform (make-hash-table) :reader genes)
   (parents :initarg :parents :accessor parents))
  (:default-initargs :parents NIL))

(defmethod shared-initialize :after ((critter genetical) slots &key genes)
  (when genes
    (if (keywordp (first genes))
        (loop for genome in genes by #'cddr
              for id in (rest genes) by #'cddr
              do (set-gene critter (gene-of genome id :error T)))
        (loop for gene in genes do (set-gene critter gene)))))

(defmethod print-object ((critter genetical) stream)
  (print-unreadable-object (critter stream :type T :identity T)
    (loop for gene in (gene-list critter)
          do (format stream " ~a: ~a (~a)" (genome gene) (id gene) (value gene)))))

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
  (gethash genome (genes critter)))

(defmethod set-gene ((critter genetical) (gene gene))
  (setf (gethash (genome gene) (genes critter)) gene))

(defmethod gene-list ((critter genetical))
  (alexandria:hash-table-values (genes critter)))

(defmethod cross ((critter genetical) (other genetical))
  ;; TODO: Skipping generations.
  (let ((genomes (genomes critter)))
    (loop for genome in (genomes other)
          unless (gene critter genome)
          do (push genome genomes))
    (loop for genome in genomes
          for inherited = (cross (gene critter genome) (gene other genome))
          when inherited collect inherited)))
