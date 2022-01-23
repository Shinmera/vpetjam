(in-package #:org.shirakumo.fraf.vpetjam)

(define-pool vpetjam)
(define-pool sound :base "sound/")
(define-pool music :base "music/")

(defmacro define-track (name file &body options)
  `(define-asset (music ,name) trial-harmony:sound
       ,file
     ,@options
     :repeat T
     :mixer :music
     :voice-class 'harmony:music-segment))

(define-track main #p"main.ogg"
  :volume 0.1)

(define-assets-from-path (vpetjam sprite-data "sprite/*.lisp" :ignore-directory T))

(define-assets-from-path (vpetjam image "texture/*.png" :ignore-directory T)
  (T :min-filter :linear-mipmap-linear :mag-filter :linear))

(define-assets-from-path (sound trial-harmony:sound "**/*.wav")
  (spawn :volume 0.3)
  (pick :volume 0.2)
  (T :volume 0.4))
