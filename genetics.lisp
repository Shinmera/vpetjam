(in-package #:org.shirakumo.fraf.vpetjam)

(defclass genetical ()
  ((genes :initform (make-hash-table) :reader genes)
   (parents :initarg :parents :accessor parents))
  (:default-initargs :parents NIL))

(defmethod shared-initialize :after ((critter genetical) slots &key genes)
  (if genes
      (loop for genome in genes by #'cddr
            for value in (rest genes) by #'cddr
            do (set-gene critter genome value))
      (loop for genome in (genomes)
            do (set-gene critter genome (gene critter genome)))))

(defmethod print-object ((critter genetical) stream)
  (print-unreadable-object (critter stream :type T :identity T)
    (loop for gene in (gene-list critter)
          do (format stream " ~a (~a)" (genome gene) (value gene)))))

(defmethod gene ((critter genetical) genome)
  (declare (type keyword genome))
  (gethash genome (genes critter) (gene-initial genome)))

(defmethod set-gene ((critter genetical) genome value)
  (declare (type number value))
  (setf (gethash genome (genes critter)) value))

(defmethod gene-list ((critter genetical))
  (alexandria:hash-table-plist (genes critter)))

(defmethod cross ((critter genetical) (other genetical))
  ;; TODO: Skipping generations.
  (loop for genome in (genomes)
        collect genome
        collect (cross-gene genome (gene critter genome) (gene critter genome))))
