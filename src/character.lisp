(in-package #:prejam-2024)


(ecs:defcomponent target
  (entity -1 :type ecs:entity))

(ecs:defcomponent movement
  (target-x 0.0 :type single-float)
  (target-y 0.0 :type single-float))

(ecs:defcomponent follows-path)

(ecs:defcomponent path-point
  (x 0.0 :type pos)
  (y 0.0 :type pos)
  (traveller -1 :type ecs:entity :index path-points))

(ecs:defcomponent character
  (team 0 :type bit :index team :documentation "0 = defender, 1 = attacker")
  (vision-range 0.0 :type single-float)
  (attack-range 0.0 :type single-float)
  (movement-speed 0.0 :type single-float))

(ecs:defsystem mortify-characters
  (:components-ro (health character)
   :components-rw (sprite)
   :when (not (plusp health-points)))
  "This parrot is no more. It has ceased to be."
  (setf sprite-name :dead
        sprite-sequence-name :dead)
  (delete-health entity)
  (delete-character entity)
  (dolist (arrow (stuck-arrows entity))
    (ecs:delete-entity arrow)))

(declaim (inline distance))
(defun distance (x1 y1 x2 y2)
  (flet ((sqr (x) (* x x)))
    (declare (inline sqr))
    (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2))))))

(declaim (inline distance*)
         (ftype (function (pos pos pos pos) single-float) distance*))
(defun distance* (x1 y1 x2 y2)
  (flet ((sqr (x) (* x x)))
    (declare (inline sqr))
    (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

(defconstant +sqrt2+ (sqrt 2))

(declaim (inline heuristic-cost))
(defun heuristic-cost (x1 y1 x2 y2)
  "Octile distance"
  (declare (type single-float x1 y1 x2 y2))
  (let* ((arbitrary-scaling-coefficient 100)
         (dx (/ (abs (the single-float (- x1 x2)))
                (* +window-width+ arbitrary-scaling-coefficient)))
         (dy (/ (abs (the single-float (- y1 y2)))
                (* +window-height+ arbitrary-scaling-coefficient))))
    (declare (type single-float dx dy))
    (+ (* +sqrt2+ (min dx dy))
       (abs (- dx dy)))))

(declaim (type (simple-array single-float) +neighbours-x+))
(define-constant +neighbours-x+
    (make-array 8 :element-type 'single-float
                  :initial-contents
                (let* ((size +scaled-tile-size+)
                       (-size (- +scaled-tile-size+)))
                  `(,size ,size 0.0 ,-size ,-size ,-size 0.0 ,size)))
  :test #'equalp)

(declaim (type (simple-array single-float) +neighbours-y+))
(define-constant +neighbours-y+
    (make-array 8 :element-type 'single-float
                  :initial-contents
                (let* ((size +scaled-tile-size+)
                       (-size (- +scaled-tile-size+)))
                  `(0.0 ,-size ,-size ,-size 0.0 ,size ,size ,size)))
  :test #'equalp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-let+-expansion (&stack (x) :once-only? nil)
    `(let ((,x ,let-plus::value))
       (declare (dynamic-extent ,x))
       ,@let-plus::body)))

(declaim (type (simple-array single-float) *cost-so-far*))
(define-global-parameter *cost-so-far*
    (make-array 0 :element-type 'single-float))

(declaim (type (simple-array fixnum) *came-from*))
(define-global-parameter *came-from*
    (make-array 0 :element-type 'fixnum))

(declaim
 (ftype
  (function
   (single-float single-float single-float single-float) (or fixnum null))
  a*))
(defun a* (start-x start-y goal-x goal-y)
  (let+ (((&values start-x* start-y*) (tile-start start-x start-y))
         ((&values goal-x* goal-y*) (tile-start goal-x goal-y))
         (world-width *world-width*)
         (world-size (the array-length (* world-width *world-height*)))
         ((&macrolet cost-so-far (x y)
            `(aref *cost-so-far*
                   (+ (the array-index
                           (* (floor ,y +scaled-tile-size+) world-width))
                      (floor ,x +scaled-tile-size+)))))
         ((&stack frontier-items)
          (make-array 512 :element-type 'fixnum))
         ((&stack frontier-priorities)
          (make-array 512 :element-type 'single-float))
         ((&stack frontier) (make-queue frontier-items frontier-priorities)))
    (when (< (length *cost-so-far*) world-size)
      (setf *cost-so-far* (ecs::adjust-array* *cost-so-far* world-size
                                              :element-type 'single-float)))
    (when (< (length *came-from*) world-size)
      (setf *came-from* (ecs::adjust-array* *came-from* world-size
                                            :element-type 'fixnum)))
    (fill *cost-so-far* float-features:single-float-nan)
    (fill *came-from* -1)
    (queue-insert frontier (tile-hash start-x* start-y*) 0.0)
    (setf (cost-so-far start-x* start-y*) 0.0)
    (loop
      :with current :of-type fixnum
      :with current-x single-float :and current-y single-float
      :when (queue-empty-p frontier) :do (return nil)
      :do (setf current (queue-pop frontier))
          (multiple-value-setq (current-x current-y) (marshal-tile current))
      :when (and (= current-x goal-x*) (= current-y goal-y*))
        :do (return current)
      :do (loop
            :for delta-x :of-type single-float :across +neighbours-x+
            :for delta-y :of-type single-float :across +neighbours-y+
            :for next-x :of-type single-float := (+ current-x delta-x)
            :for next-y :of-type single-float := (+ current-y delta-y)
            :when (and (plusp next-x) (plusp next-y)
                       (< next-x (float +window-width+))
                       (< next-y (float +window-height+)))
            :do (let* ((next-hash (tile-hash next-x next-y))
                       (new-cost (+ (cost-so-far current-x current-y)
                                    (total-map-tile-movement-cost next-hash)
                                    (heuristic-cost delta-x delta-y 0.0 0.0)))
                       (next-cost (cost-so-far next-x next-y)))
                  (when (< new-cost +max-movement-cost+)
                    (incf new-cost (float (if (and (not (zerop delta-x))
                                                   (not (zerop delta-y)))
                                              ;; diagonal movement
                                              (+ (total-map-tile-movement-cost
                                                  (tile-hash current-x next-y))
                                                 (total-map-tile-movement-cost
                                                  (tile-hash next-x current-y)))
                                              0)))
                    (when (and (< new-cost +max-movement-cost+)
                               (or (float-features:float-nan-p next-cost)
                                   (< new-cost next-cost)))
                      (setf (cost-so-far next-x next-y) new-cost
                            (aref *came-from*
                                  (+ (the array-index
                                          (* (floor next-y +scaled-tile-size+)
                                             world-width))
                                     (floor next-x +scaled-tile-size+)))
                            (tile-hash current-x current-y))
                      (queue-insert frontier next-hash
                                    (+ new-cost (heuristic-cost
                                                 next-x next-y
                                                 goal-x* goal-y*))))))))))

(declaim (ftype (function (pos pos fixnum ecs:entity)) reconstruct-path))
(defun reconstruct-path (start-x start-y goal entity)
  (let* ((size (length *came-from*))
         (length 0)
         (path (make-array size :element-type 'fixnum)))
    (declare (dynamic-extent path)
             (type array-length size length))
    (setf (aref path length) goal)
    (loop :for c :of-type fixnum := goal
          :then (let+ (((&values x y) (marshal-tile c)))
                  (aref *came-from*
                        (+ (the array-index (* (floor y +scaled-tile-size+)
                                               *world-width*))
                           (floor x +scaled-tile-size+))))
          :until (minusp c)
          :do (setf (aref path (incf length)) c))
    (ecs:make-object
     `((:path-point :x ,start-x :y ,start-y :traveller ,entity)))
    (loop :for i :from length :downto 0
          :do (let+ (((&values x y) (marshal-tile (aref path i))))
                ;; TODO compact by comparing x and y being same with next point
                (ecs:make-object
                 `((:path-point :x ,x :y ,y :traveller ,entity)))))))
