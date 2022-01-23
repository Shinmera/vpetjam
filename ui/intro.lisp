(in-package #:org.shirakumo.fraf.vpetjam)

(defclass textbox (alloy:value-component)
  ((index :initform 0 :accessor index)))

(defmethod alloy:handle ((event alloy:pointer-down) (textbox textbox))
  ;(alloy:activate textbox)
  )

(defmethod alloy:handle ((event alloy:button-down) (textbox textbox))
                                        ;  (alloy:activate textbox)
  )

(defmethod alloy:activate ((textbox textbox))
  (cond ((< (index textbox) (1- (length (alloy:value textbox))))
         (incf (index textbox))
         (alloy:mark-for-render textbox))
        (T
         (hide-panel 'tutorial))))

(defmethod alloy:text ((textbox textbox))
  (language-string (aref (alloy:value textbox) (index textbox))))

(presentations:define-realization (ui textbox)
  ((backdrop simple:rectangle)
   (alloy:margins -10 10 10 -10)
   :pattern (colored:color 1 1 1 0.2))
  ((background simple:rectangle)
   (alloy:margins)
   :pattern (colored:color 0 0 0 0.8))
  ((text simple:text)
   (alloy:margins 50 50 50 10)
   alloy:text
   :pattern colors:white
   :font (setting :display :font)
   :wrap T
   :size (alloy:un 25)
   :halign :start
   :valign :top)
  ((label simple:text)
   (alloy:extent 10 150 (alloy:pw 1) 50)
   (@ tutorial)
   :pattern colors:white
   :font (setting :display :font)
   :size (alloy:un 30)
   :halign :start
   :valign :top
   :markup '((0 1000 (:italic T)))))

(presentations:define-update (ui textbox)
  (text
   :text alloy:text))

(defclass tutorial (menuing-panel)
  ())

(defmethod initialize-instance :after ((panel tutorial) &key)
  (let ((layout (make-instance 'org.shirakumo.alloy.layouts.constraint:layout))
        (textbox (alloy:represent #(tutorial-1 tutorial-2 tutorial-3 tutorial-4 tutorial-5) 'textbox)))
    (alloy:enter textbox layout :constraints `((:left 50) (:right 50) (:bottom 50) (:height 200)))
    (alloy:finish-structure panel layout textbox)))
