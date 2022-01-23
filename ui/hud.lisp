(in-package #:org.shirakumo.fraf.vpetjam)

(defun format-money (money)
  (format NIL "~c ~d" (code-char #xA4) money))

(defclass money-display (alloy:label)
  ())

(defmethod alloy:text ((display money-display))
  (format-money (alloy:value display)))

(presentations:define-realization (ui money-display)
  ((label simple:text)
   (alloy:margins -10)
   alloy:text
   :size (alloy:un 40)
   :font (setting :display :font)
   :pattern colors:white
   :halign :left
   :valign :bottom))

(presentations:define-update (ui money-display)
  (label
   :text alloy:text))

(defclass hud (panel)
  ())

(defmethod initialize-instance :after ((hud hud) &key (player (unit 'player T)))
  (let* ((layout (make-instance 'org.shirakumo.alloy.layouts.constraint:layout)))
    (alloy:enter (alloy:represent (money player) 'money-display)
                 layout :constraints `((:left 30) (:bottom 30) (:size 1000 50)))
    (alloy:finish-structure hud layout NIL)))
