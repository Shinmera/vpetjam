(trigger toggle-fullscreen
         (key :one-of (:f11)))

(trigger toggle-menu
         (key :one-of (:tab))
         (button :one-of (:home :start)))

(trigger screenshot
         (key :one-of (:print-screen)))

(trigger previous
         (key :one-of (:left :up :w :a))
         (button :one-of (:dpad-l :dpad-u))
         (axis :one-of (:l-h :dpad-h) :threshold -0.5)
         (axis :one-of (:l-v :dpad-v) :threshold +0.5))

(trigger next
         (key :one-of (:right :down :s :d))
         (button :one-of (:dpad-r :dpad-d))
         (axis :one-of (:l-h :dpad-h) :threshold +0.5)
         (axis :one-of (:l-v :dpad-v) :threshold -0.5))

(trigger accept
         (key :one-of (:e :enter))
         (button :one-of (:a)))

(trigger back
         (key :one-of (:esc :escape))
         (button :one-of (:b)))

(trigger interact
         (key :one-of (:e :enter :space))
         (button :one-of (:a :b :x :y)))

(trigger left
         (key :one-of (:a :left))
         (button :one-of (:dpad-l))
         (axis :one-of (:l-h :dpad-h) :threshold -0.5))

(trigger right
         (key :one-of (:d :right))
         (button :one-of (:dpad-r))
         (axis :one-of (:l-h :dpad-h) :threshold 0.5))

(trigger up
         (key :one-of (:w :up))
         (button :one-of (:dpad-u))
         (axis :one-of (:l-v :dpad-v) :threshold 0.5))

(trigger down
         (key :one-of (:s :down))
         (button :one-of (:dpad-d))
         (axis :one-of (:l-v :dpad-v) :threshold -0.5))
