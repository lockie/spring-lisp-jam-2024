(in-package #:prejam-2024)


(ecs:defcomponent poison
  (duration 0.0 :type single-float)
  (dps 0 :type fixnum :documentation "Damage per second"))

(ecs:defsystem intoxicate
  (:components-rw (poison animation-state)
   :arguments ((:dt single-float)))
  (let ((previous-duration poison-duration))
    (decf poison-duration dt)
    (unless (plusp poison-duration)
      (setf animation-state-tint 0)
      (return-from ecs::current-entity (delete-poison entity)))
    (unless (= (the fixnum (truncate previous-duration))
               (the fixnum (truncate poison-duration)))
      (when (has-health-p entity)
        (make-damage entity poison-dps)))))

(defun make-poisoned (entity &key (dps 1) (duration 10.0))
  (assign-poison entity :duration duration :dps dps)
  (setf (animation-state-tint entity) #x00ff00ff)
  entity)
