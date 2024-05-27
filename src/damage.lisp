(in-package #:cycle-of-evil)


(ecs:defcomponent health
    (points 0 :type fixnum)
  (max-points points :type fixnum))

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
                  (if (plusp damage-number-damage)
                      (al:map-rgba-f 1.0 0.0 0.0 dissipation)
                      (al:map-rgba-f 0.0 1.0 0.0 dissipation))
                  (- position-x (/ +scaled-tile-size+ 4))
                  (+ position-y (* (- dissipation 1.5) +scaled-tile-size+))
                  0
                  (princ-to-string (abs damage-number-damage)))))

(ecs:defsystem render-health-bars
  (:components-ro (position health)
   :with (half-tile :of-type single-float := (/ +scaled-tile-size+ 2)))
  (let ((health (/ (float health-points) health-max-points)))
    (al:draw-rectangle
     (- (ftruncate position-x) half-tile -2)
     (- (ftruncate position-y) half-tile 10)
     (+ (ftruncate position-x) half-tile -2)
     (- (ftruncate position-y) half-tile 18)
     +black+ 1)
    (al:draw-filled-rectangle
     (- (ftruncate position-x) half-tile -2)
     (- (ftruncate position-y) half-tile 10)
     (+ (ftruncate position-x) (* health half-tile) -3)
     (- (ftruncate position-y) half-tile 17)
     `(al::r ,(- 1.0 health) al::g ,health al::b 0.0 al::a 1.0))))

(declaim (ftype (function (ecs:entity positive-fixnum)) make-damage))
(defun make-damage (entity damage)
  (let ((damage-taken (round (* damage (character-defense-multiplier entity)))))
    (declare (type fixnum damage-taken))
    (decf (health-points entity) damage-taken)
    (with-position () entity
      (ecs:make-object `((:parent :entity ,entity)
                         (:position :x ,x :y ,y)
                         (:damage-number :damage ,damage-taken))))))

(defun make-healing (entity healing)
  (with-health () entity
    (let* ((new-points (min max-points (+ points healing)))
           (healed (- new-points points)))
      (setf points new-points)
      (with-position () entity
      (ecs:make-object `((:parent :entity ,entity)
                         (:position :x ,x :y ,y)
                         (:damage-number :damage ,(- healed))))))))
