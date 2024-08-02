(in-package #:cycle-of-evil)


(ecs:defcomponent target
  (entity -1 :type ecs:entity))

(ecs:defcomponent movement
  (target-x 0.0 :type single-float)
  (target-y 0.0 :type single-float))

(ecs:defcomponent path
  "A path previously calculated by A* algorithm."
  (target-x 0.0 :type single-float)
  (target-y 0.0 :type single-float))

(ecs:defcomponent path-point
  (x 0.0 :type float-coordinate)
  (y 0.0 :type float-coordinate)
  (traveller -1 :type ecs:entity :index path-points))

(ecs:defsystem mortify-characters
  (:components-ro (health character position)
   :components-rw (sprite animation-state)
   :when (not (plusp health-points)))
  "This parrot is no more. It has ceased to be."
  (when (eq sprite-name :barrel-red)
    ;; delete fuse sound effect
    (dolist (child (children entity))
      (when (has-sound-p child)
        (ecs:delete-entity child))))
  (make-sound-effect entity
                     (case character-team
                       (0 :death)
                       (1 :monster-death))
                     position-x position-y)
  (setf sprite-name :dead
        sprite-sequence-name :dead
        animation-state-tint 0)
  (delete-health entity)
  (delete-character entity)
  (when (has-behavior-p entity)
    (delete-behavior-tree
     (behavior-type entity)
     entity)
    (delete-behavior entity))
  (dolist (arrow (stuck-arrows entity))
    (ecs:delete-entity arrow)))

(defconstant +sqrt2+ (sqrt 2))

(a*:define-path-finder find-path (entity has-path-p)
    (:world-size (the array-length (* *world-width* *world-height*))
     :indexer (a*:make-row-major-indexer *world-width*
                  :node-width +scaled-tile-size+
                  :node-height +scaled-tile-size+)
     :goal-reached-p same-tile-p
     :neighbour-enumerator
     (lambda (x y f)
       (let+ (((&values cx cy) (tile-center x y)))
         (funcall
          (a*:make-8-directions-enumerator
           :node-width +scaled-tile-size+ :node-height +scaled-tile-size+
           :max-x +window-width+ :max-y +window-height+)
          cx cy f)))
     :exact-cost
     (lambda (x1 y1 x2 y2)
       (let+ (((&flet cost (x y)
                 (total-map-tile-movement-cost
                  (multiple-value-call #'encode-float-coordinates
                    (tile-start x y)))))
              (diagonal-cost (if (and (/= x1 x2) (/= y1 y2))
                                 (+ (cost x1 y2) (cost x2 y1))
                                 0.0)))
         (+ (cost x2 y2) diagonal-cost)))
     :heuristic-cost (a*:make-octile-distance-heuristic 9.765625e-6)
     :max-movement-cost +max-movement-cost+
     :path-initiator
     (lambda (length)
       (declare (ignore length))
       (when has-path-p
         (dolist (point (path-points entity))
           (ecs:delete-entity point)))
       (assign-path entity :target-x start-x :target-y start-y)
       (ecs:make-object
        `((:path-point :x ,start-x :y ,start-y :traveller ,entity)
          (:parent :entity ,entity))))
     :path-processor
     (lambda (x y)
       (with-position (current-x current-y) entity
         (unless (and (= current-x x) (= current-y y))
           (ecs:make-object
            `((:path-point :x ,x :y ,y :traveller ,entity)
              (:parent :entity ,entity)))))))
  "Find path for given entity.")
