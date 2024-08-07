(in-package #:cycle-of-evil)


(ecs:defcomponent meat
  (points 0 :type fixnum)
  (carry -1 :type ecs:entity :index meat-carried-by))

(ecs:defsystem setup-behaviors
  (:components-ro (behavior)
   :components-no (behavior-tree-marker))
  (make-behavior-tree behavior-type entity))

(define-behavior-tree-node (idle
                            :components-rw (sprite))
    ()
  "Sets sprite animation to idle."
  (setf sprite-sequence-name :idle)
  (complete-node t))

(define-behavior-tree-node (wander-off
                            :components-ro (position))
    ()
  (let+ (((&values sx sy) (tile-start position-x position-y))
         (possible-locations nil))
    (funcall (a*:make-8-directions-enumerator
              :node-width +scaled-tile-size+ :node-height +scaled-tile-size+
              :min-x +scaled-tile-size+ :min-y +scaled-tile-size+
              :max-x +window-width+ :max-y +window-height+)
             sx sy
             (lambda (next-x next-y)
               (let* ((next-hash (encode-float-coordinates next-x next-y))
                      (cost (total-map-tile-movement-cost next-hash)))
                 (when (< cost +max-movement-cost+)
                   (push next-hash possible-locations)))))
    (when possible-locations
      (let+ ((location (random-elt possible-locations))
             ((&values target-x target-y)
              (decode-float-coordinates location)))
        (assign-movement entity :target-x target-x :target-y target-y)))
    (complete-node possible-locations)))

(define-behavior-tree-node (defender-team
                            :components-ro (character))
    ()
  "Succeeds if character belongs to defender team."
  (complete-node (zerop character-team)))

(define-behavior-tree-node (full-health
                            :components-ro (health))
    ()
  (complete-node (= health-points health-max-points)))

(define-behavior-tree-node (pick-snack
                            :components-ro (position character))
    ()
  "Picks the nearest meat as a target. Fails if there is no meat nearby."
  (flet ((sqr (x) (* x x)))
    (loop
      :with snacks :of-type list := (shuffle (meat-carried-by -1))
      :for snack :of-type ecs:entity :in snacks
      :for distance := (with-position (snack-x snack-y) snack
                         (distance* position-x position-y snack-x snack-y))
      :when (<= distance (sqr character-vision-range))
      :do (assign-target entity :entity snack)
          (return-from ecs::current-entity (complete-node t))
      :finally (complete-node nil))))

(define-behavior-tree-node (test-snack
                            :components-ro (target))
  ()
  (complete-node (and (has-meat-p target-entity)
                      (not (ecs:entity-valid-p (meat-carry target-entity))))))

(define-behavior-tree-node (eat-snack
                            :components-rw (target health))
    ()
  (unless (has-meat-p target-entity)
    ;; already eaten?
    (return-from ecs::current-entity (complete-node nil)))
  (if (has-poison-p target-entity)
      (with-poison () target-entity
        (make-poisoned entity :dps dps :duration duration))
      (make-healing entity (meat-points target-entity)))
  (delete-meat target-entity)
  (delete-sprite target-entity)
  (delete-animation-sequence target-entity)
  (delete-animation-state target-entity)
  (delete-image target-entity)
  (delete-target entity)
  (complete-node t))

(define-behavior-tree-node (pick-nearest-enemy
                            :components-ro (position character)
                            :with (teams := (vector (team 0) (team 1))))
    ()
  "Picks the nearest enemy as a target. Fails if there are no enemies nearby."
  (flet ((sqr (x) (* x x)))
    (loop
      :with nearest-enemy :of-type ecs:entity := -1
      :with nearest-enemy-distance := most-positive-single-float
      :for enemy :of-type ecs:entity :in (aref teams (logxor character-team 1))
      :for distance := (with-position (enemy-x enemy-y) enemy
                         (distance* position-x position-y enemy-x enemy-y))
      :when (and (<= distance (sqr character-vision-range))
                 (< distance nearest-enemy-distance))
      :do (setf nearest-enemy-distance distance
                nearest-enemy enemy)
      :finally (cond ((minusp nearest-enemy)
                      (complete-node nil))
                     (t
                      (assign-target entity :entity enemy)
                      (complete-node t))))))

(define-behavior-tree-node (pick-random-enemy
                            :components-ro (position character))
    ()
  "Picks an enemy at random. Fails if there are no enemies nearby."
  (flet ((sqr (x) (* x x)))
    (loop
      :with enemies :of-type list := (shuffle (team (logxor character-team 1)))
      :for enemy :of-type ecs:entity :in enemies
      :for distance := (with-position (enemy-x enemy-y) enemy
                         (distance* position-x position-y enemy-x enemy-y))
      :when (<= distance (sqr character-vision-range))
      :do (assign-target entity :entity enemy)
          (return-from ecs::current-entity (complete-node t))
      :finally (complete-node nil))))

(define-behavior-tree-node (calculate-path
                            :components-ro (position target))
    ()
  "Calculates and caches the path points using A* algorithm."
  (with-position (target-x target-y) (target-entity entity)
    (let ((has-path-p (has-path-p entity)))
      (if (or (not has-path-p)
              (not (same-tile-p (path-target-x entity)
                                (path-target-y entity)
                                target-x target-y)))
            (complete-node
             (find-path position-x position-y target-x target-y
                        :entity entity
                        :has-path-p has-path-p))
          (complete-node t)))))

(declaim (inline approx-equal))
(defun approx-equal (a b &optional (epsilon 0.01))
  (< (abs (- a b)) epsilon))

(define-behavior-tree-node (follow-path
                            :components-rw (position))
    ()
  "Follows path previously calculated by A* algorithm."
  (let ((path-points (path-points entity :count 2)))
    (if-let (first-point (first path-points))
      (with-path-point (point-x point-y) first-point
        (if (approx-equal 0 (distance* position-x position-y point-x point-y))
            (block point-reached
              (setf position-x point-x
                    position-y point-y
                    position-tile
                    (encode-float-coordinates position-x position-y))
              (ecs:delete-entity first-point)
              (if-let (next-point (second path-points))
                (with-path-point (next-point-x next-point-y) next-point
                  (assign-movement entity :target-x next-point-x
                                          :target-y next-point-y)
                  (complete-node t))
                (block path-completed
                  (complete-node nil))))
            (block point-not-reached
              (unless (has-movement-p entity)
                (dolist (point (path-points entity))
                  (ecs:delete-entity point))))))
      (complete-node nil))))

(define-behavior-tree-node (move
                            :components-ro (movement)
                            :components-rw (position sprite animation-state)
                            :arguments ((:dt single-float)))
    ((speed 0.0 :type single-float)
     (animation-sequence :run :type keyword))
  "Moves towards the target."
  (if (approx-equal 0 (distance* position-x position-y
                                 movement-target-x movement-target-y))
      (block finished
        (setf position-x movement-target-x
              position-y movement-target-y
              position-tile (encode-float-coordinates position-x position-y))
        (delete-movement entity)
        (complete-node t))
      (let+ (((&flet sqr (x) (* x x)))
             (diffx (- movement-target-x position-x))
             (diffy (- movement-target-y position-y))
             (max-delta (sqrt (+ (sqr diffx) (sqr diffy))))
             (delta (min (* dt move-speed) max-delta))
             (angle (atan diffy diffx))
             (dx (* delta (cos angle)))
             (dy (* delta (sin angle)))
             (newx (+ position-x dx))
             (newy (+ position-y dy)))
        (declare (type single-float diffx diffy delta angle dx dy newx newy))
        (if (obstaclep newx newy)
            (block struck
              (warn "character ~a stuck @ ~a ~a~%" entity newx newy)
              (setf sprite-sequence-name :idle)
              (delete-movement entity)
              (complete-node nil))
            (setf position-x newx
                  position-y newy
                  position-tile (encode-float-coordinates position-x position-y)
                  sprite-sequence-name move-animation-sequence
                  animation-state-flip (if (minusp dx) 1 0))))))

(define-behavior-tree-node (test-target-near
                            :components-ro (position target))
    ((range 0.0 :type single-float))
  "Succeeds if entity's target is within given range."
  (flet ((sqr (x) (* x x)))
    (with-position (target-x target-y) target-entity
      (complete-node
       (and (<= (distance* position-x position-y target-x target-y)
                (sqr test-target-near-range)))))))

(define-behavior-tree-node (test-target-alive
                            :components-ro (target))
    ()
  "Succeeds if entity's target is still alive."
  (complete-node (has-health-p target-entity)))

(define-behavior-tree-node (attack
                            :components-ro (position target character
                                                     animation-sequence)
                            :components-rw (sprite animation-state))
    ((started 0 :type bit)
     (done 0 :type bit
             :documentation "Attack happened, but animation's still playing"))
  (with-position (target-x target-y) target-entity
    (if (zerop attack-started)
        (let* ((dx (- target-x position-x))
               (dy (- target-y position-y))
               (abs-dx (abs dx))
               (abs-dy (abs dy))
               (flip (minusp dx))
               (sequence (cond ((> abs-dx abs-dy) :attack-right)
                               ((plusp dy)        :attack-down)
                               (t                 :attack-up))))
          (when (eq sprite-name :warrior-blue)
            ;; NOTE warrior has alternating attack animations
            (setf sequence (format-symbol :keyword "~a-~a" sequence
                                          (1+ (random 2)))))
          (when (eq sprite-name :barrel-red)
            (make-sound-effect entity :fuse position-x position-y
                               :variations 1))
          (setf sprite-sequence-name sequence
                animation-state-flip (if flip 1 0)
                attack-started 1))
        (cond ((and (zerop attack-done)
                    (= animation-state-frame
                       (- animation-sequence-frames
                          ;; NOTE this abomination stems from different
                          ;; animation lengths of characters in tileset
                          (cond ((and (plusp character-melee-attack)
                                      (plusp character-splash-attack)) 1)
                                ((plusp character-melee-attack)        3)
                                ((plusp character-splash-attack)       5)
                                (t                                     2)))))
               (cond ((zerop character-melee-attack)
                      (make-sound-effect
                       (make-projectile-object position-x position-y
                                               target-x target-y sprite-name
                                               (randint character-damage-min
                                                        character-damage-max)
                                               character-projectile-speed
                                               character-splash-attack
                                               animation-state-flip)
                       (if (eq sprite-name :archer-blue) :shoot :throw)
                       position-x position-y))
                     ((plusp character-fire-damage)
                      (make-damage target-entity
                                   (randint character-damage-min
                                            character-damage-max))
                      (make-on-fire target-entity :dps character-fire-damage)
                      (make-sound-effect entity :wood position-x position-y))
                     ((plusp character-splash-attack)
                      (make-explosion-effects position-x position-y
                                              (randint character-damage-min
                                                       character-damage-max)))
                     (t
                      (make-damage target-entity
                                   (randint character-damage-min
                                            character-damage-max))
                      (make-sound-effect entity :sword position-x position-y)))
               (setf attack-done 1))

              ((and (plusp animation-state-finished)
                    (= animation-state-frame (1- animation-sequence-frames)))
               (setf sprite-sequence-name :idle)
               (complete-node t))))))

(define-behavior-tree-node (wait
                            :arguments ((:dt single-float)))
    ((time 1.0 :type single-float :documentation "Wait time in seconds."))
  (if (plusp wait-time)
      (decf wait-time dt)
      (complete-node t)))

(define-behavior-tree offensive
    ((repeat :name "root")
     ((fallback)
      ((sequence :name "snack")
       ((defender-team))
       ((invert)
        ((full-health)))
       ((pick-snack))
       ((repeat-until-fail)
        ((sequence :name "grab-snack")
         ((test-snack))
         ((invert)
          ((test-target-near :range +scaled-tile-size+)))
         ((calculate-path))
         ((follow-path))
         ((move :speed (character-movement-speed entity)))))
       ((test-snack))
       ((eat-snack)))
      ((sequence)
       ((pick-random-enemy))
       ((invert)
        ((repeat-until-fail)
         ((sequence  :name "deal-with-enemy")
          ((sequence)
           ((repeat-until-fail)
            ((sequence :name "pursuit")
             ((test-target-alive))
             ((invert)
              ((test-target-near :range (character-attack-range entity))))
             ((calculate-path))
             ((follow-path))
             ((move :speed (character-movement-speed entity)))))
           ((test-target-alive))
           ((test-target-near :range (character-attack-range entity))))
          ((repeat-until-fail)
           ((sequence :name "attacks")
            ((test-target-alive))
            ((test-target-near :range (character-attack-range entity)))
            ((attack))
            ((wait :time (character-attack-cooldown entity)))))
          ((invert)
           ((test-target-near :range (character-attack-range entity))))))))
      ((invert)
       ((idle)))
      ((invert)
       ((sequence :name "wander-off")
        ((random :probability 0.01))
        ((wander-off))
        ((move :speed (character-movement-speed entity)))
        ((idle)))))))
