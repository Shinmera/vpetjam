(in-package #:org.shirakumo.fraf.vpetjam)

(defclass genetical ()
  ((genes :initform (make-hash-table) :reader genes)))

(defmethod shared-initialize :after ((critter genetical) slots &key genes)
  (loop for genome in *genomes*
        unless (eql genome :growth)
        do (set-gene critter genome (gene-initial genome)))
  (when genes
    (loop for genome in genes by #'cddr
          for value in (rest genes) by #'cddr
          do (set-gene critter genome value)))
  (unless (gene critter :growth)
    (set-gene critter :growth (gene-growth (gene critter :body)))))

(defmethod print-object ((critter genetical) stream)
  (print-unreadable-object (critter stream :type T :identity T)
    (loop for gene in (gene-list critter)
          do (format stream " ~a (~a)" (genome gene) (value gene)))))

(defmethod gene ((critter genetical) genome)
  (declare (type keyword genome))
  (gethash genome (genes critter)))

(defmethod set-gene ((critter genetical) genome value)
  (declare (type keyword genome))
  (declare (type (or keyword number null) value))
  (let ((value
          (if value
              (cond
                ((eql value :random)
                 (gene-random genome :include-initial (eql genome :body)))
                ((keywordp value)
                 (if (eql genome :speed)
                     (gene-random-speed value)
                     (error "Invalid gene value for ~a: ~a" genome value)))
                (T value))
              (gene-initial genome))))
    (setf (gethash genome (genes critter)) value)))

(defmethod gene-list ((critter genetical))
  (loop for genome in *genomes*
        collect genome
        collect (gene critter genome)))

(defmethod cross ((critter genetical) (other genetical))
  (loop for genome in *genomes*
        collect genome
        collect (cross-gene genome (gene critter genome) (gene critter genome))))

(defmethod price-multiplier ((critter genetical))
  (loop with multiplier = 1.0
        for genome in *genomes*
        for gene-multiplier = (gene-price-multiplier genome (gene critter genome))
        do (setf multiplier (* multiplier gene-multiplier))
        finally (return multiplier)))
