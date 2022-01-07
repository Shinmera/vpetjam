(in-package #:org.shirakumo.fraf.vpetjam)

(defmethod handle ((ev quit-game) (controller controller)))

(define-action-set in-menu (exclusive-action-set))
(define-action skip (in-menu))
(define-action advance (in-menu))
(define-action previous (in-menu))
(define-action next (in-menu))
(define-action accept (in-menu))
(define-action back (in-menu))

(define-action toggle-menu ())
(define-action screenshot ())
(define-action toggle-fullscreen ())

(define-action-set in-game (exclusive-action-set))
(define-action interact (in-game))
(define-action jump (in-game))
(define-action left (in-game analog-action))
(define-action right (in-game analog-action))
(define-action up (in-game analog-action))
(define-action down (in-game analog-action))
