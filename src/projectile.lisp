(in-package #:cycle-of-evil)


(ecs:defcomponent projectile
  (target-x 0.0 :type float-coordinate)
  (target-y 0.0 :type float-coordinate)
  (angle 0.0 :type single-float)
  (speed 0.0 :type single-float)
  (damage 0 :type fixnum)
  (splash 0 :type bit :documentation "Whether projectile deals splash damage.
NOTE: assuming splash damage = explosion"))

(ecs:defcomponent stuck-arrow
  "Then I took an arrow in the knee..."
  (adventurer -1 :type ecs:entity :index stuck-arrows))

(ecs:defcomponent explosion)

(ecs:defsystem move-projectiles
  (:components-ro (projectile animation-state)
   :components-rw (position)
   :arguments ((:dt single-float)))
  (let+ (((&flet sqr (x) (* x x)))
         (diffx (- projectile-target-x position-x))
         (diffy (- projectile-target-y position-y))
         (max-delta (sqrt (+ (sqr diffx) (sqr diffy))))
         (delta (min (* dt projectile-speed) max-delta))
         (dx (* delta (cos projectile-angle)))
         (dy (* delta (sin projectile-angle)))
         (newx (+ position-x dx))
         (newy (+ position-y dy)))
    (setf position-x newx
          position-y newy
          position-tile (encode-float-coordinates newx newy))))

(ecs:defsystem impact-projectiles
  (:components-ro (projectile position sprite animation-state))
  (when (< (distance* projectile-target-x projectile-target-y
                      position-x position-y)
           (* +scaled-tile-size+ +scaled-tile-size+))
    (if (zerop projectile-splash)
        (with-tiles (encode-float-coordinates
                     projectile-target-x projectile-target-y)
          object
          (when (has-health-p object)
            (make-damage object projectile-damage)
            (ecs:make-object
             `((:parent :entity ,object)
               (:position :x ,projectile-target-x
                          :y ,projectile-target-y)
               (:sprite :name ,sprite-name
                        :sequence-name :projectile-stuck)
               (:animation-state :rotation ,animation-state-rotation)
               (:stuck-arrow :adventurer ,object)))
            (make-sound-effect object :arrow position-x position-y)
            (loop-finish)))
        (make-explosion-effects
         projectile-target-x projectile-target-y projectile-damage))
    (ecs:delete-entity entity)))

(declaim (ftype (function (float-coordinate float-coordinate positive-fixnum)
                          (values ecs:entity &optional))
                make-explosion-effects))
(defun make-explosion-effects (x y damage)
  (with-tiles (encode-float-coordinates x y) object
    (when (has-health-p object)
      (make-damage object damage))
    (when (has-fire-p object)
      (setf (fire-duration object) 0.0)))
  (make-sound-effect *current-map* :explosion x y :variations 1)
  (ecs:make-object
   `((:parent :entity ,*current-map*)
     (:position :x ,x :y ,y)
     (:sprite :name :explosions :sequence-name :explosion)
     (:explosion))))

(ecs:defsystem move-stuck-arrows
  (:components-ro (stuck-arrow)
   :components-rw (position))
  (with-position (adventurer-x adventurer-y) stuck-arrow-adventurer
    (setf position-x adventurer-x
          position-y adventurer-y
          position-tile (encode-float-coordinates position-x position-y))))

(ecs:defsystem stop-explosions
  (:components-ro (explosion animation-sequence animation-state))
  (when (= animation-state-frame animation-sequence-frames)
    (ecs:delete-entity entity)))

(declaim (ftype
          (function (float-coordinate
                     float-coordinate
                     float-coordinate
                     float-coordinate
                     keyword positive-fixnum single-float bit bit)
                    (values ecs:entity &optional))
          make-projectile-object))
(defun make-projectile-object (x y target-x target-y sprite
                               damage speed splash flip)
  (let* ((angle (atan (- target-y y)
                      (- target-x x)))
         (splashp (plusp splash))
         (object (ecs:make-object
                  `((:parent :entity ,*current-map*)
                    (:position :x ,x
                               :y ,y)
                    (:sprite :name ,sprite
                             :sequence-name :projectile)
                    (:projectile :target-x ,target-x
                                 :target-y ,target-y
                                 :angle ,angle
                                 :speed  ,speed
                                 :damage ,damage
                                 :splash ,splash)
                    (:animation-state :rotation ,(if splashp
                                                     0.0
                                                     angle)
                                      :flip ,(if splashp
                                                 (logxor flip 1)
                                                 0))))))
    (when splashp
      (make-sound-effect object :fuse x y :repeat t :variations 1))
    object))
