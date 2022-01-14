(in-package #:org.shirakumo.fraf.vpetjam)

(defun genomes ()
  (list :hue :speed :face :body :hat))

(defun gene-initial (genome)
  (declare (type keyword genome))
  (case genome
    (:hat 0)
    (T (alexandria:random-elt (gene-potential genome)))))

(defun gene-potential (genome)
  (declare (type keyword genome))
  (case genome
    (:speed '(0.2 0.5 1.0 3.0 5.0 10.0)) ;; Might be any decimal from 0 to 20 (inclusive).
    (:hue '(0.0 1.5 3.0 4.5 6.0)) ;; Might be any decimal from 0 to 2 * pi (exclusive).
    (:body (loop for i from 0 below 8 collect i))
    (:face (loop for i from 0 below 8 collect i))
    (:hat (loop for i from 0 below 5 collect i))))

(defun cross-gene (genome gene-a gene-b)
  (declare (type keyword genome))
  (declare (type (or null number) gene-a gene-b))
  (when (or (null gene-a) (null gene-b))
    (return-from cross-gene (or gene-a gene-b)))
  (case genome
    (:hue (angle-midpoint gene-a gene-b))
    (:speed
     (let ((medium 3.0)) ;; FIXME: Should this be in a global parameter?
       (clamp 0.0 (+ medium (* 0.8 (+ (- gene-a medium) (- gene-b medium)))) 20.0)))
    (:hat
     (if (< (random 1.0) 0.2) ;; FIXME: Same as above. Also is this a good value?
         (alexandria:random-elt (remove 0 (gene-potential :hat)))
         (if (< (random 1.0) 0.5) gene-a gene-b)))
    (T (if (< (random 1.0) 0.5) gene-a gene-b))))
