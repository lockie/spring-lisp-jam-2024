(in-package #:cycle-of-evil)


(define-constant +window-width+ 1280)
(define-constant +window-height+ 800)

(define-constant +black+ (al:map-rgb 0 0 0)
  :test #'equalp)

(ecs:defcomponent parent
  (entity -1 :type ecs:entity :index children))

(ecs:defcomponent character
  (team 0 :type bit :index team :documentation "0 = defender, 1 = attacker")
  (vision-range 0.0 :type single-float)
  (attack-range 0.0 :type single-float)
  (movement-speed 0.0 :type single-float)
  (melee-attack 0 :type bit)
  (splash-attack 0 :type bit)
  (projectile-speed 0.0 :type single-float)
  (damage-min 0 :type fixnum)
  (damage-max 0 :type fixnum)
  (defense-multiplier
      1.0 :type single-float
          :documentation "Incoming damage is multiplied by this value")
  (fire-damage
   0 :type fixnum
     :documentation "If non-zero, character's attacks ignite with this DPS")
  (attack-cooldown 0.0 :type single-float))

(ecs:hook-up ecs:*entity-deleting-hook*
             (lambda (entity)
               (dolist (child (children entity))
                 (ecs:delete-entity child))))

(ecs:defcomponent behavior
  (type :|| :type keyword))

(ecs:defcomponent map
  (tint nil :type list :documentation "Global map tint color."))

(defvar *map-descriptions*)

(defparameter *current-progress* 0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type single-float +scale-factor+ +tile-size+ +scaled-tile-size+))
  (defconstant +scale-factor+ 0.5)
  (defconstant +tile-size+ 64.0)
  (defconstant +scaled-tile-size+ (* +tile-size+ +scale-factor+)))

(declaim (inline distance))
(defun distance (x1 y1 x2 y2)
  (flet ((sqr (x) (* x x)))
    (declare (inline sqr))
    (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2))))))

(declaim (inline distance*)
         (ftype (function (float-coordinate
                           float-coordinate
                           float-coordinate
                           float-coordinate)
                          single-float)
                distance*))
(defun distance* (x1 y1 x2 y2)
  (flet ((sqr (x) (* x x)))
    (declare (inline sqr))
    (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

(declaim (inline tile-start)
         (ftype (function (float-coordinate float-coordinate)
                          (values float-coordinate float-coordinate))
                tile-start))
(defun tile-start (x y)
  (values
   (* +scaled-tile-size+ (the fixnum (floor x +scaled-tile-size+)))
   (* +scaled-tile-size+ (the fixnum (floor y +scaled-tile-size+)))))

(declaim (inline tile-center)
         (ftype (function (float-coordinate float-coordinate)
                          (values float-coordinate float-coordinate))
                tile-center))
(defun tile-center (x y)
  (values
   (* +scaled-tile-size+ (+ 0.5 (the fixnum (floor x +scaled-tile-size+))))
   (* +scaled-tile-size+ (+ 0.5 (the fixnum (floor y +scaled-tile-size+))))))

(declaim (inline same-tile-p)
         (ftype (function (float-coordinate
                           float-coordinate
                           float-coordinate
                           float-coordinate)
                          boolean)
                same-tile-p))
(defun same-tile-p (x1 y1 x2 y2)
  (and (= (floor x1 +scaled-tile-size+)
          (floor x2 +scaled-tile-size+))
       (= (floor y1 +scaled-tile-size+)
          (floor y2 +scaled-tile-size+))))

(ecs:defcomponent position
  "The object position in pixels."
  (x 0.0 :type float-coordinate
         :documentation "x position aka screen pixel coordinate")
  (y 0.0 :type float-coordinate
         :documentation "y position aka screen pixel coordinate")
  (tile (encode-float-coordinates x y)
        :type fixnum :index tiles
        :documentation "Tile index, for fast map tile lookups."))

(ecs:defcomponent size
  "Unscaled object size in pixels."
  (width  0.0 :type float-coordinate)
  (height 0.0 :type float-coordinate))

(ecs:defcomponent image
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer))

(defun ensure-relative (pathname)
  (let ((dirs (rest (pathname-directory pathname))))
    (make-pathname :directory (when dirs
                                (list* :relative dirs))
                   :defaults pathname)))

(defun resource-path (filename)
  (merge-pathnames (ensure-relative filename) #P"../Resources/"))

(declaim (ftype (function (string) string) kebabize)
         (inline kebabize))
(defun kebabize (name)
  (substitute #\- #\Space
              (substitute #\- #\_ name)))

(declaim (type array-length *world-width* *world-height*))
(define-global-parameter *world-width* 0 "World width in tiles")
(define-global-parameter *world-height* 0 "World height in tiles")

(declaim (inline randint)
         (ftype (function (fixnum fixnum) fixnum) randint))
(defun randint (start end)
  (+ start (random (+ 1 (- end start)))))
