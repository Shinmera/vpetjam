(asdf:defsystem vpetjam
  :version "0.0.0"
  :build-operation "deploy-op"
  :build-pathname #+linux "vpetjam-linux.run"
  #+darwin "vpetjam-macos.o"
  #+win32 "vpetjam-windows"
  #+(and bsd (not darwin)) "vpetjam-bsd.run"
  #-(or linux bsd win32) "vpetjam"
  :entry-point "org.shirakumo.fraf.vpetjam::main"
  :components ((:file "package")
               (:file "toolkit")
               (:file "actions")
               (:file "sprite-data")
               (:file "assets")
               (:file "helpers")
               (:file "camera")
               (:file "world")
               (:file "farm")
               (:file "player")
               (:file "cheats")
               (:file "main")
               (:module "ui"
                :components ((:file "general"))))
  :serial T
  :defsystem-depends-on (:deploy)
  :depends-on (:trial-glfw
               :trial-alloy
               :trial-harmony
               :trial-notify
               :alloy-constraint
               :alexandria
               :cl-mixed-wav
               :cl-mixed-vorbis))
