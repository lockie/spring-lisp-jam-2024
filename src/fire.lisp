(in-package #:cycle-of-evil)


(ecs:defcomponent fire
  (duration 0.0 :type single-float)
  (dps 0 :type fixnum :documentation "Damage per second"))

(ecs:defcomponent fire-effect
  (character -1 :type ecs:entity :index flames :unique t))

(ecs:defsystem spread-fire
  (:components-ro (fire position))
  (with-tiles (encode-float-coordinates position-x position-y) object
    (when (and (/= object entity)
               (not (has-fire-p object))
               (not (has-fire-effect-p object))
               (has-character-p object))
      (make-on-fire object :dps (1+ (random fire-dps))))))

(ecs:defsystem move-fire-effect
  (:components-ro (fire-effect)
   :components-rw (position))
  (with-position (character-x character-y) fire-effect-character
    (setf position-x character-x
          position-y character-y
          position-tile (encode-float-coordinates position-x position-y))))

(ecs:defsystem burn
  (:components-rw (fire)
   :arguments ((:dt single-float)))
  (let ((previous-duration fire-duration))
    (decf fire-duration dt)
    (unless (plusp fire-duration)
      (ecs:delete-entity (flames entity))
      (return-from ecs:current-entity (delete-fire entity)))
    (unless (= (the fixnum (truncate previous-duration))
               (the fixnum (truncate fire-duration)))
      (when (has-health-p entity)
        (make-damage entity fire-dps)))))

(defun make-on-fire (entity &key (dps 1) (duration 3.9))
  (assign-fire entity :duration duration :dps dps)
  (let ((already-burning (flames entity :missing-error-p nil)))
    (unless (ecs:entity-valid-p already-burning)
      (with-position () entity
        (make-sound-effect
         (ecs:make-object
          `((:parent :entity ,entity)
            (:position :x ,x :y ,y)
            (:fire-effect :character ,entity)
            (:sprite :name :fire :sequence-name :fire)))
         :fire x y :repeat t :variations 1)))))
