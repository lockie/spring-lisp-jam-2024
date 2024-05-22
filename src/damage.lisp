(in-package #:prejam-2024)


(ecs:defcomponent health
  (points 0 :type fixnum))

(defconstant +damage-numbers-display-time+ 2.0)

(ecs:defcomponent damage-number
  (damage 0 :type fixnum)
  (display-time +damage-numbers-display-time+ :type single-float))

(ecs:defsystem disspate-damage-numbers
  (:components-rw (damage-number)
   :arguments ((:dt single-float)))
  (decf damage-number-display-time dt)
  (when (minusp damage-number-display-time)
    (ecs:delete-entity entity)))

(defvar *damage-numbers-font*)

(ecs:defsystem render-damange-numbers
  (:components-ro (position damage-number))
  (let ((dissipation (/ damage-number-display-time
                        +damage-numbers-display-time+)))
    (al:draw-text *damage-numbers-font*
                  (al:map-rgba-f 1.0 0.0 0.0 dissipation)
                  position-x
                  (+ position-y (* (- dissipation 1.5) +scaled-tile-size+))
                  0
                  (princ-to-string damage-number-damage))))

(declaim (ftype (function (ecs:entity positive-fixnum)) make-damage))
(defun make-damage (entity damage)
  (decf (health-points entity) damage)
  (with-position () entity
    (ecs:make-object `((:position :x ,x :y ,y)
                       (:damage-number :damage ,damage)))))
