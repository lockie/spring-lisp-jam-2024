(in-package #:prejam-2024)


(ecs:defcomponent behavior
  (type :|| :type keyword))

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
              position-tile (tile-hash position-x position-y)
              sprite-sequence-name :idle)
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

(define-behavior-tree simple
    ((repeat :name "root")
     ((fallback)
      ((sequence)
       ((pick-nearest-enemy))
       ((fallback)
        ((sequence)
         ((calculate-path))
         ((repeat-until-fail)
          ((sequence)
           ((follow-path))
           ((move)))))))
      ((idle)))))
