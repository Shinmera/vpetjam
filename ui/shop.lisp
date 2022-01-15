(in-package #:org.shirakumo.fraf.vpetjam)

(defclass buy-button (alloy:direct-value-component alloy:button)
  ((player :initarg :player :accessor player)
   (initargs :initarg :initargs :accessor initargs)
   (cost :initarg :cost :accessor cost)))

(defmethod alloy:activate ((button buy-button))
  (let ((player (player button)))
    (when (<= (cost button) (money player))
      (let ((seed (apply #'make-instance 'seed :location (vcopy (location player))
                                               :height 64 :hvel 1.0 :velocity (vec 0 -5)
                         (initargs button))))
        (decf (money player) (cost button))
        (enter-and-load seed (container player) +main+)))))

(presentations:define-realization (ui buy-button)
  ((:background simple:rectangle)
   (alloy:margins))
  ((border simple:rectangle)
   (alloy:margins)
   :line-width (alloy:un 1))
  ((label simple:text)
   (alloy:margins 5 10 10 5)
   alloy:text
   :font (setting :display :font)
   :halign :start
   :valign :middle
   :size (alloy:un 20)
   :pattern colors:black)
  ((cost simple:text)
   (alloy:margins 5 10 10 5)
   (format-money (cost alloy:renderable))
   :font (setting :display :font)
   :halign :end
   :valign :middle
   :size (alloy:un 20)
   :pattern colors:black))

(presentations:define-update (ui buy-button)
  (:background
   :pattern (if alloy:focus colors:white (colored:color 1 1 1 0.1)))
  (border
   :pattern (if alloy:focus colors:transparent colors:white))
  (label)
  (cost))

(presentations:define-animated-shapes button
  (:background (simple:pattern :duration 0.2))
  (border (simple:pattern :duration 0.3)))

(animation:define-animation not-enough-cash
  0.1 ((setf simple:pattern) colors:red)
  0.5 ((setf simple:pattern) colors:white))

(defclass shop (menuing-panel pausing-panel)
  ())

(defmethod initialize-instance :after ((panel shop) &key (player (unit 'player T)))
  (let* ((layout (make-instance 'eating-constraint-layout))
         (focus (make-instance 'alloy:focus-list))
         (clipper (make-instance 'alloy:clip-view :limit :x))
         (scroll (alloy:represent-with 'alloy:y-scrollbar clipper))
         (list (make-instance 'alloy:vertical-linear-layout
                              :shapes (list (simple:rectangle (unit 'ui-pass T) (alloy:margins) :pattern (colored:color 1 1 1 0.75)))
                              :min-size (alloy:size 100 50)))
         (money (alloy:represent (slot-value player 'money) 'money-display)))
    (alloy:enter list clipper)
    (alloy:enter clipper layout :constraints `((:left 50) (:right 70) (:bottom 100) (:top 100)))
    (alloy:enter scroll layout :constraints `((:width 20) (:right 50) (:bottom 100) (:top 100)))
    (alloy:enter money layout :constraints `((:left 50) (:above ,clipper 10) (:size 1000 50)))
    (loop for (name cost . initargs) in '((basic-seed 90 :genes (:body :random))
                                          (personal-seed 100 :genes (:body :random :face :random))
                                          (hued-seed 130 :genes (:body :random :hue :random))
                                          (slow-seed 150 :genes (:body :random :speed :slow))
                                          (fast-seed 200 :genes (:body :random :speed :fast))
                                          (hat-seed 250 :genes (:body :random :hat :random)))
          for button = (make-instance 'buy-button :value name :player player :initargs initargs :cost cost)
          do (alloy:enter button list)
             (alloy:enter button focus)
             (alloy:on alloy:activate (button)
               (when (< (money player) (cost button))
                 (alloy:with-unit-parent money
                   (animation:apply-animation 'not-enough-cash (presentations:find-shape 'label money))))))
    (let ((back (make-instance 'button :value (@ go-backwards-in-ui) :on-activate (lambda () (hide panel)))))
      (alloy:enter back layout :constraints `((:left 50) (:bottom 30) (:size 200 50)))
      (alloy:enter back focus)
      (alloy:on alloy:exit (focus)
        (setf (alloy:focus back) :strong)))
    (alloy:finish-structure panel layout focus)))
