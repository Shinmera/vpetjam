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
  (when (or (null gene) (null other))
    (return-from cross (or gene other)))
  (case (genome gene)
    (:hue
     (make-gene :hue NIL (angle-midpoint (value gene) (value other))
                (if (< (random 1.0) 0.5) (recessive gene) (recessive other))))
    (:speed
     (let* ((medium (value (gene-of :speed :medium)))
            (speed (+ medium (* 0.8 (+ (- (value gene) medium) (- (value other) medium))))))
       (make-gene :speed NIL (clamp 0.0 speed 20.0)
                  (if (< (random 1.0) 0.5) (recessive gene) (recessive other)))))
    (:hat
     (let* ((hats (genes-of :hat :error T))
            (count-a (floor (length hats) 2))
            (count-b count-a))
       (unless (eql (recessive gene) (recessive other))
         (let* ((recessive-count (floor (* (length hats) 0.375)))
                (dominant-count (- (length hats) recessive-count)))
           (if (recessive gene)
               (setf count-a recessive-count
                     count-b dominant-count)
               (setf count-a dominant-count
                     count-b recessive-count))))
       (let ((gene (alexandria:random-elt
                    (append (make-list count-a :initial-element gene)
                            (make-list count-b :initial-element other)
                            hats))))
         gene)))
    (T
     (if (eql (recessive gene) (recessive other))
         (if (< (random 1.0) 0.5) gene other)
         (if (< (random 1.0) 0.375)
             (if (recessive gene) gene other)
             (if (recessive gene) other gene))))))

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
