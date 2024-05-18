(in-package #:prejam-2024)


(define-constant +window-width+ 1280)
(define-constant +window-height+ 800)

(defvar *font*)

(ecs:defcomponent parent
  (entity -1 :type ecs:entity :index children))

(ecs:hook-up ecs:*entity-deleting-hook*
             (lambda (entity)
               (dolist (child (children entity))
                 (ecs:delete-entity child))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type single-float +scale-factor+ +tile-size+ +scaled-tile-size+))
  (defconstant +scale-factor+ 0.5)
  (defconstant +tile-size+ 64.0)
  (defconstant +scaled-tile-size+ (* +tile-size+ +scale-factor+)))

;; NOTE: negative map coords are not supported
(deftype pos () '(single-float 0f0 #.(/ +tile-size+ single-float-epsilon)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline tile-start)
           (ftype (function (pos pos) (values pos pos)) tile-start))
  (defun tile-start (x y)
    (values
     (* +scaled-tile-size+ (the fixnum (floor x +scaled-tile-size+)))
     (* +scaled-tile-size+ (the fixnum (floor y +scaled-tile-size+)))))

  (declaim
   (inline tile-hash)
   (ftype (function (pos pos) fixnum) tile-hash))
  (defun tile-hash (x y)
    (let+ (((&values tile-x tile-y) (tile-start x y))
           (x* (truncate tile-x))
           (y* (truncate tile-y)))
      (declare (type (integer 0 2147483647) x* y*))
      (logior (ash x* 31) y*))))

(declaim (inline marshal-tile)
         (ftype (function (fixnum) (values pos pos)) marshal-tile))
(defun marshal-tile (hash)
  (values
   (float (ash hash -31))
   (float (logand hash 2147483647))))

(ecs:defcomponent position
  "The object position in pixels."
  (x 0.0 :type pos
         :documentation "x position aka screen pixel coordinate")
  (y 0.0 :type pos
         :documentation "y position aka screen pixel coordinate")
  (tile (tile-hash x y)
        :type fixnum :index tiles
        :documentation "Tile index, for fast map tile lookups."))

(ecs:defcomponent size
  "Unscaled object size in pixels."
  (width  0.0 :type pos)
  (height 0.0 :type pos))

(ecs:defcomponent image
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer))

(defun ensure-relative (pathname)
  (let ((dirs (rest (pathname-directory pathname))))
    (make-pathname :directory (when dirs
                                (list* :relative dirs))
                   :defaults pathname)))

(defun resource-path (filename)
  (merge-pathnames (ensure-relative filename) #P"../Resources/"))

(declaim (type array-length *world-width* *world-height*))
(define-global-parameter *world-width* 0 "World width in tiles")
(define-global-parameter *world-height* 0 "World height in tiles")
