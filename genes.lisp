(in-package #:org.shirakumo.fraf.vpetjam)

(defparameter *genomes* '(:hue :speed :face :body :hat))

(defun gene-initial (genome)
  (declare (type keyword genome))
  (case genome
    (:speed 3.0)
    (:hue 0.0)
    (T 0)))

(defun gene-potential (genome)
  (declare (type keyword genome))
  (case genome
    (:speed '(0.2 0.5 1.0 3.0 5.0 10.0)) ;; Might be any decimal from 0 to 20 (inclusive).
    (:hue '(0.0 1.5 3.0 4.5 6.0)) ;; Might be any decimal from 0 to 2 * pi (exclusive).
    (:body (loop for i from 0 below 8 collect i))
    (:face (loop for i from 0 below 8 collect i))
    (:hat (loop for i from 0 below 5 collect i))))

(defun gene-random (genome &key include-initial)
  (declare (type keyword genome))
  (let ((potential (if include-initial
                       (gene-potential genome)
                       (remove (gene-initial genome) (gene-potential genome)))))
    (alexandria:random-elt potential)))

(defun gene-random-speed (speed)
  (declare (type keyword speed))
  (let ((default (gene-initial :speed)))
    (ecase speed
      (:slow (alexandria:random-elt (mapcar #'(lambda (x) (< x default)) (gene-potential :speed))))
      (:fast (alexandria:random-elt (mapcar #'(lambda (x) (< default x)) (gene-potential :speed)))))))

(defun cross-gene (genome gene-a gene-b)
  (declare (type keyword genome))
  (declare (type (or null number) gene-a gene-b))
  (let ((gene-a (or gene-a (gene-initial :genome)))
        (gene-b (or gene-b (gene-initial :genome))))
    (case genome
      (:hue (angle-midpoint gene-a gene-b))
      (:speed
       (let ((medium (gene-initial :speed)))
         (clamp 0.0 (+ medium (* 0.8 (+ (- gene-a medium) (- gene-b medium)))) 20.0)))
      (T (if (< (random 1.0) 0.5) gene-a gene-b)))))

(defun gene-price-multiplier (genome value)
  (declare (type keyword genome))
  (declare (type (or null number) value))
  (let* ((initial (gene-initial genome))
         (value (or value initial)))
    (case genome ;; Initial values give 1.0 multiplier.
      (:speed
       (cond ;; These just make nice graphs.
         ;; Assuming the initial value is 3.0, then
         ;; 0.0 gives ~2.95 multiplier and 20.0 gives ~3.89 multiplier.
         ((< initial value) (1+ (log (- value (1- initial)))))
         ((< value initial)
          (let ((adjustment (+ initial 0.5)))
            (1+ (log (/ (/ (+ value 0.5) adjustment))))))
         (T 1.0)))
      (:hue ;; Hue is the distance from the default value, so a maximum of pi.
       (1+ (abs (- (abs (- pi initial)) (abs (- pi value))))))
      (:face (if (/= initial value) 1.5 1.0)) ;; Any non-standard gives 1.5.
      (:hat (if (/= initial value) 2.5 1.0)) ;; Any non-standard gives 2.5.
      (T 1.0)))) ;; Nothing special about vegetable types.
