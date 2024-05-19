(in-package #:prejam-2024)


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
                            :components-ro (position character)
                            :with (teams := (vector (shuffle (team 0))
                                                    (shuffle (team 1)))))
    ()
  "Picks an enemy at random. Fails if there are no enemies nearby."
  (flet ((sqr (x) (* x x)))
    (loop
      :for enemy :of-type ecs:entity :in (aref teams (logxor character-team 1))
      :for distance := (with-position (enemy-x enemy-y) enemy
                         (distance* position-x position-y enemy-x enemy-y))
      :when (<= distance (sqr character-vision-range))
      :do (assign-target entity :entity enemy)
          (return-from ecs::current-entity (complete-node t))
      :finally (complete-node nil))))

(define-behavior-tree-node (calculate-path
                            :components-ro (position target))
    ()
  "Calculates the path points using A* algorithm."
  (with-position (target-x target-y) (target-entity entity)
    (let ((goal (a* position-x position-y target-x target-y)))
      (when goal
        (assign-follows-path entity)
        (reconstruct-path position-x position-y goal entity))
      (complete-node goal))))

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
                    position-tile (tile-hash position-x position-y))
              (ecs:delete-entity first-point)
              (if-let (next-point (second path-points))
                (with-path-point (next-point-x next-point-y) next-point
                  (assign-movement entity :target-x next-point-x
                                          :target-y next-point-y)
                  (complete-node t))
                (block path-completed
                  (delete-follows-path entity)
                  (complete-node nil))))
            (block point-not-reached
              (unless (has-movement-p entity)
                (delete-follows-path entity)
                (dolist (point (path-points entity))
                  (ecs:delete-entity point))))))
      (complete-node nil))))

(define-behavior-tree-node (move
                            :components-ro (character movement)
                            :components-rw (position sprite animation-state)
                            :arguments ((:dt single-float)))
    ()
  "Moves towards the target."
  (if (approx-equal 0 (distance* position-x position-y
                                 movement-target-x movement-target-y))
      (block finished
        (setf position-x movement-target-x
              position-y movement-target-y
              position-tile (tile-hash position-x position-y))
        (delete-movement entity)
        (complete-node t))
      (let+ (((&flet sqr (x) (* x x)))
             (diffx (- movement-target-x position-x))
             (diffy (- movement-target-y position-y))
             (max-delta (sqrt (+ (sqr diffx) (sqr diffy))))
             (delta (min (* dt character-movement-speed) max-delta))
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
                  position-tile (tile-hash position-x position-y)
                  sprite-sequence-name :run
                  animation-state-flip (if (minusp dx) 1 0))))))

(define-behavior-tree-node (test-attack-range
                            :components-ro (position character target))
    ()
  "Succeeds if entity's target is within attack range."
  (flet ((sqr (x) (* x x)))
    (with-position (target-x target-y) target-entity
      (complete-node
       (and (has-health-p target-entity)
            (<= (distance* position-x position-y target-x target-y)
                (sqr character-attack-range)))))))

(declaim (ftype (function (keyword keyword) boolean) keyword-prefix-p)
         (inline keyword-prefix-p))
(defun keyword-prefix-p (prefix string)
  "Does STRING begin with PREFIX? Code from UIOP:STRING-PREFIX-P"
  (let* ((x (string prefix))
         (y (string string))
         (lx (length x))
         (ly (length y)))
    (and (<= lx ly) (string= x y :end2 lx))))

(defmacro set-attack-animation ()
  `(with-position (target-x target-y) target-entity
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
       (setf sprite-sequence-name sequence
             animation-state-flip (if flip 1 0)))))

(define-behavior-tree-node (melee-attack
                            :components-ro (animation-sequence position target)
                            :components-rw (animation-state sprite))
    ((done 0 :type bit))
  (if (not (keyword-prefix-p :attack sprite-sequence-name))
      (set-attack-animation)
      (cond ((and (= animation-state-frame (- animation-sequence-frames 3))
                  (zerop melee-attack-done))
             (make-damage target-entity (1+ (random 10)))
             (setf melee-attack-done 1))

            ((and (= animation-state-frame (1- animation-sequence-frames))
                  (plusp animation-state-finished))
             (setf sprite-sequence-name :idle)
             (complete-node t)))))

(define-behavior-tree-node (ranged-attack
                            :components-ro (animation-sequence position target)
                            :components-rw (animation-state sprite))
    ((done 0 :type bit)
     (splash 0 :type bit))
  (if (not (keyword-prefix-p :attack sprite-sequence-name))
      (set-attack-animation)
      (cond ((and (= animation-state-frame
                     ;; NOTE this abomination stems from different animation
                     ;;  lengths for different characters in tileset
                     (- animation-sequence-frames
                        (+ 2 (* 3 ranged-attack-splash))))
                  (zerop ranged-attack-done))
             (with-position (target-x target-y) target-entity
               (let ((angle (atan (- target-y position-y)
                                  (- target-x position-x))))
                 (ecs:make-object
                  `((:position :x ,position-x
                               :y ,position-y)
                    (:sprite :name ,sprite-name
                             :sequence-name :projectile)
                    (:projectile :target-x ,target-x
                                 :target-y ,target-y
                                 :angle ,angle
                                 :speed 100.0
                                 :splash ,ranged-attack-splash)
                    (:animation-state :rotation
                                      ,(if (plusp ranged-attack-splash)
                                           0.0
                                           angle)
                                      :flip
                                      ,(if (plusp ranged-attack-splash)
                                           (logxor animation-state-flip 1)
                                           0))))))
             (setf ranged-attack-done 1))

            ((and (= animation-state-frame (1- animation-sequence-frames))
                  (plusp animation-state-finished))
             (setf sprite-sequence-name :idle)
             (complete-node t)))))

(define-behavior-tree-node (wait
                            :arguments ((:dt single-float)))
    ((time 1.0 :type single-float :documentation "Wait time in seconds."))
  (if (plusp wait-time)
      (decf wait-time dt)
      (complete-node t)))

(define-behavior-tree simple-melee
    ((repeat :name "root")
     ((fallback)
      ((sequence)
       ((pick-random-enemy))
       ((fallback)
        ((invert)
         ((repeat-until-fail)
          ((sequence :name "attack")
           ((test-attack-range))
           ((melee-attack))
           ((wait :time 0.15)))))
        ((sequence :name "pursuit")
         ((calculate-path))
         ((repeat-until-fail)
          ((sequence)
           ((follow-path))
           ((move))
           ((invert)
            ((test-attack-range))))))))
      ((idle)))))

(define-behavior-tree simple-ranged
  ((repeat :name "root")
     ((fallback)
      ((sequence)
       ((pick-random-enemy))
       ((fallback)
        ((invert)
         ((repeat-until-fail)
          ((sequence :name "attack")
           ((test-attack-range))
           ((ranged-attack))
           ((wait :time 0.3)))))
        ((sequence :name "pursuit")
         ((calculate-path))
         ((repeat-until-fail)
          ((sequence)
           ((follow-path))
           ((move))
           ((invert)
            ((test-attack-range))))))))
      ((idle)))))

(define-behavior-tree simple-thrower
  ((repeat :name "root")
     ((fallback)
      ((sequence)
       ((pick-random-enemy))
       ((fallback)
        ((sequence :name "attack")
         ((test-attack-range))
         ((ranged-attack :splash 1))
         ((wait :time 0.3)))
        ((sequence :name "pursuit")
         ((calculate-path))
         ((repeat-until-fail)
          ((sequence)
           ((follow-path))
           ((move))
           ((invert)
            ((test-attack-range))))))))
      ((idle)))))
