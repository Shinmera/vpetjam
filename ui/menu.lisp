(in-package #:org.shirakumo.fraf.vpetjam)

(define-shader-entity fullscreen-background (textured-entity trial:fullscreen-entity)
  ((texture :initform (// 'vpetjam 'main-menu))))

(defclass main-menu-button (button)
  ())

(presentations:define-realization (ui main-menu-button)
  ((:label simple:text)
   (alloy:margins) alloy:text
   :font (setting :display :font)
   :halign :middle :valign :middle)
  ((:border simple:rectangle)
   (alloy:extent 0 0 (alloy:pw 1) 1)))

(presentations:define-update (ui main-menu-button)
  (:label
   :size (alloy:un 16)
   :pattern colors:white)
  (:border
   :pattern (if alloy:focus colors:white colors:transparent)))

(presentations:define-animated-shapes main-menu-button
  (:border (simple:pattern :duration 0.2)))

(define-asset (vpetjam logo-rect) mesh
    (make-rectangle 800 300))

(define-shader-entity logo (textured-entity vertex-entity located-entity)
  ((vertex-array :initform (// 'vpetjam 'logo-rect))
   (texture :initform (// 'vpetjam 'logo))))

(defclass main-menu (menuing-panel)
  ())

(defmethod initialize-instance :after ((panel main-menu) &key)
  (let ((layout (make-instance 'org.shirakumo.alloy.layouts.constraint:layout))
        (menu (make-instance 'alloy:vertical-linear-layout :cell-margins (alloy:margins 5) :min-size (alloy:size 100 30)))
        (focus (make-instance 'alloy:focus-list)))
    (alloy:enter menu layout :constraints `((:center :w) (:bottom 20) (:height 300) (:width 350)))
    (macrolet ((with-button ((name &rest initargs) &body body)
                 `(make-instance 'main-menu-button :value (@ ,name) :on-activate (lambda ()
                                                                                   (discard-events +world+)
                                                                                   ,@body)
                                                   :focus-parent focus :layout-parent menu ,@initargs)))
      (with-button (new-game)
        (hide panel)
        (change-scene +main+ (setup-world (make-instance 'world)))
        (show-panel 'hud))
      (with-button (credits-menu)
        (show-panel 'credits))
      (let ((subbutton
              (with-button (wishlist-cta)
                (open-in-browser "https://store.steampowered.com/app/1261430/Kandria/?utm_source=in-game"))))
        (alloy:on alloy:focus (value subbutton)
          (setf (presentations:update-overrides subbutton)
                (if value
                    `((:label :markup ((0 1000 (:rainbow T)))))
                    `((:label :markup ()))))))
      (let ((exit (with-button (exit-game)
                    (quit *context*))))
        (alloy:on alloy:exit (focus)
          (setf (alloy:focus exit) :weak)
          (setf (alloy:focus focus) :strong))))
    (alloy:finish-structure panel layout focus)))

(defmethod show :after ((menu main-menu) &key)
  (enter-and-load (make-instance 'fullscreen-background) +world+ +main+)
  (enter-and-load (make-instance 'logo :location (vec 0 80 0)) +world+ +main+))
