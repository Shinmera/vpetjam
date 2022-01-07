(defpackage #:vpetjam
  (:nicknames #:org.shirakumo.fraf.vpetjam)
  (:use #:cl+trial)
  (:shadow #:main #:launch
           #:located-entity #:sized-entity #:animated-sprite
           #:camera #:sprite-data)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:trial-alloy #:org.shirakumo.fraf.trial.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:presentations #:org.shirakumo.alloy.renderers.simple.presentations)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:animation #:org.shirakumo.alloy.animation)
   (#:gamepad #:org.shirakumo.fraf.gamepad)
   (#:harmony #:org.shirakumo.fraf.harmony.user)
   (#:trial-harmony #:org.shirakumo.fraf.trial.harmony)
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:notify #:org.shirakumo.fraf.trial.notify)
   (#:bvh #:org.shirakumo.fraf.trial.bvh2))
  (:export
   #:launch))
