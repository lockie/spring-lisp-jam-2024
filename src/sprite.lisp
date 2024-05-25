(in-package #:prejam-2024)


(ecs:defcomponent (animation-prefab
                   :composite-index (animation-prefab
                                     (sprite-name sequence-name)
                                     :unique t))
  "Animated sprite prefab to copy data from."
  (sprite-name :|| :type keyword :documentation "Sprite name")
  (sequence-name :|| :type keyword :documentation "Animation sequence name"))

(ecs:defcomponent sprite
  "Animated sprite."
  (name :|| :type keyword :documentation "Sprite name")
  (sequence-name :idle :type keyword :documentation "Animation sequence name"))

(ecs:defcomponent animation-sequence
  "Animated sprite sequence."
  (name :|| :type keyword :documentation "Animation sequence name")
  (sprite-name :|| :type keyword :documentation "Sprite name")
  (frames 0 :type fixnum :documentation "Number of frames in animation")
  (layers 0 :type fixnum :documentation "Number of layers in sprite")
  (frame-duration 0.0 :type single-float
                      :documentation "Each frame duration, in seconds.
                      NOTE: assuming duration is the same for all frames")
  (repeat 0 :type bit :documentation "Whether to repeat animation"))

(ecs:defcomponent animation-state
  "Sprite animation state."
  (frame 0 :type fixnum
           :documentation "Current animation frame")
  (time 0.0 :type single-float
            :documentation "Time elapsed from the beginning of frame, seconds")
  (flip 0 :type bit
          :documentation "Flip sprite horizontally so character looks left")
  (rotation 0.0 :type single-float
            :documentation "Rotation (mostly used for projectiles)")
  (finished 0 :type bit :documentation "Only for cases with repeat=0")
  (tint 0 :type fixnum :documentation "RGBA value for tint, 0 for no tint"))

(define-constant +transparent+ (al:map-rgba 255 255 255 255)
  :test #'equalp)

(ecs:defsystem render-sprites
  (:components-ro (position size image animation-sequence animation-state)
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (loop :for layer :of-type fixnum :from 0 :below animation-sequence-layers
        :with x-offset := (* animation-state-frame size-width)
        :with tint :=
           (if (zerop animation-state-tint)
               +transparent+
               `(al::r ,(float (ldb (byte 8 24) animation-state-tint))
                 al::g ,(float (ldb (byte 8 16) animation-state-tint))
                 al::b ,(float (ldb (byte 8  8) animation-state-tint))
                 al::a ,(float (ldb (byte 8  0) animation-state-tint))))
        :for y-offset := (* layer size-height)
        :for scaled-width := (* +scale-factor+ size-width)
        :for scaled-height := (* +scale-factor+ size-height)
        :do (al:draw-tinted-scaled-rotated-bitmap-region
             image-bitmap
             x-offset y-offset
             size-width size-height
             (if (zerop layer) ;; no tint on shadow, which is usually layer 0
                 +transparent+
                 tint)
             (* 0.5 size-width) (* 0.5 size-height)
             position-x position-y
             +scale-factor+ +scale-factor+
             animation-state-rotation
             (if (zerop animation-state-flip)
                 0 :flip-horizontal)))
  ;; (al:draw-circle position-x position-y 2 (al:map-rgb 0 255 255) 2)
  ;; (al:draw-rectangle
  ;;  (- position-x (* 0.5 (* +scale-factor+ size-width)))
  ;;  (- position-y (* 0.5 (* +scale-factor+ size-height)))
  ;;  (+ (- position-x (* 0.5 (* +scale-factor+ size-width)))
  ;;     (* +scale-factor+ size-width))
  ;;  (+ (- position-y (* 0.5 (* +scale-factor+ size-height)))
  ;;     (* +scale-factor+ size-height))
  ;;  (al:map-rgb 255 0 0)
  ;;  1)
  )

(ecs:defsystem update-sprites
  (:components-ro (animation-sequence)
   :components-rw (animation-state)
   :arguments ((:dt single-float)))
  (incf animation-state-time dt)
  (when (> animation-state-time animation-sequence-frame-duration)
    (multiple-value-bind (nframes rest-time)
        (floor animation-state-time animation-sequence-frame-duration)
      (declare (type non-negative-fixnum nframes))
      (setf animation-state-time rest-time)
      (multiple-value-bind (repeat frame)
          (truncate (+ animation-state-frame nframes) animation-sequence-frames)
        (setf animation-state-frame (cond
                                      ((zerop repeat)
                                       frame)
                                      ((zerop animation-sequence-repeat)
                                       (setf animation-state-finished 1)
                                       (1- animation-sequence-frames))
                                      (t
                                       frame)))))))

(ecs:defsystem change-sprites-animation
  (:components-ro (sprite)
   :components-rw (animation-sequence)
   :when (not (and (eq animation-sequence-sprite-name sprite-name)
                   (eq animation-sequence-name sprite-sequence-name))))
  (let ((prefab (animation-prefab :sprite-name sprite-name
                                  :sequence-name sprite-sequence-name)))
    ;; TODO : for prefab=nil case, restart with list of loaded sprites
    (replace-animation-sequence entity prefab)
    (replace-image entity prefab)
    (replace-size entity prefab)
    (assign-animation-state entity
                            :frame 0
                            :time 0.0
                            :flip (if (has-animation-state-p entity)
                                      (animation-state-flip entity)
                                      0)
                            :finished 0)))

(ecs:defsystem setup-sprites
  (:components-ro (sprite)
   :components-no (animation-sequence))
  (assign-animation-sequence entity))

(cffi:defcfun memcpy :pointer
  (dst :pointer)
  (src :pointer)
  (size :unsigned-long))

(defun load-sprite-file (full-filename)
  (loop
    :with name := (make-keyword (string-upcase (kebabize
                                                (pathname-name full-filename))))
    :with sprite := (ase:load-sprite (al:make-binary-stream full-filename))
    :with sprite-width := (ase:sprite-width sprite)
    :with sprite-height := (ase:sprite-height sprite)
    :with layers := (ase:sprite-layers sprite)
    :with nlayers := (floor (length layers) 2)
    :with duration :of-type fixnum := 0
    :for tag :across (ase:sprite-tags sprite)
    :for first-frame := (ase:tag-first-frame tag)
    :for last-frame := (ase:tag-last-frame tag)
    :for tag-length := (- last-frame first-frame -1)
    :for tag-name := (ase:tag-name tag)
    :for sequence-name := (make-keyword (string-upcase (kebabize tag-name)))
    :for tag-repeat := (ase:tag-repeat tag)
    :for bitmap-width := (* tag-length sprite-width)
    :for bitmap-height := (* nlayers sprite-height)
    :for bitmap := (al:create-bitmap bitmap-width bitmap-height)
    :for bitmap-lock := (al:lock-bitmap bitmap :abgr-8888 :readwrite)
    :for row :of-type fixnum := 0
    :do (doplist (layer-name layer layers)
                 (loop
                   :for frame :from first-frame :upto last-frame
                   :for col :of-type fixnum :from 0
                   :for cel := (aref (ase:layer-cels layer) frame)
                   :when cel
                   :do (cffi:with-foreign-slots
                           ((al::data al::pitch al::pixel-size)
                            bitmap-lock
                            (:struct al::locked-region))
                         (loop
                           :with cel-data := (ase:cel-data cel)
                           :with cel-width := (ase:cel-width cel)
                           :with cel-width-bytes := (* 4 cel-width)
                           :with start-x := (+ (* sprite-width col)
                                               (ase:cel-x cel))
                           :with start-y := (+ (* sprite-height row)
                                               (ase:cel-y cel))
                           :with end-y := (+ start-y (ase:cel-height cel))
                           :for y :from start-y :below end-y
                           :for dst := (cffi:inc-pointer
                                        al::data
                                        (+ (* 4 start-x) (* y al::pitch)))
                           :do (cffi:with-pointer-to-vector-data (src cel-data)
                                 (memcpy dst
                                         (cffi:inc-pointer
                                          src (* (- y start-y) cel-width-bytes))
                                         cel-width-bytes))))
                       (setf duration (ase:cel-duration cel))
                   :finally (incf row)))
        (al:unlock-bitmap bitmap)
        (ecs:make-object
         `((:image :bitmap ,bitmap)
           (:size :width ,(float sprite-width)
                  :height ,(float sprite-height))
           (:animation-prefab :sprite-name ,name
                              :sequence-name ,sequence-name)
           (:animation-sequence :name ,sequence-name
                                :sprite-name ,name
                                :frames ,tag-length
                                :layers ,nlayers
                                :frame-duration ,(/ duration 1000.0)
                                :repeat ,(if (zerop tag-repeat) 1 0))))))

(defun load-sprite (filename)
  (load-sprite-file (resource-path filename)))

(cffi:defcallback process-sprite-file :int
    ((file (:pointer (:struct al::fs-entry))) (data :pointer))
  (declare (ignore data))
  (if (zerop (logand (al::get-fs-entry-mode file)
                     (cffi:foreign-bitfield-value 'al::file-mode '(:isdir))))
      (progn
        (load-sprite-file (al::get-fs-entry-name file))
        (cffi:foreign-enum-value 'al::for-each-fs-entry-result :ok))
      (cffi:foreign-enum-value 'al::for-each-fs-entry-result :skip)))

(defun load-sprites ()
  (let ((sprites-dir (al:create-fs-entry
                      (namestring (resource-path "sprites")))))
    (unwind-protect
         (= (cffi:foreign-enum-value 'al::for-each-fs-entry-result :ok)
            (al::for-each-fs-entry sprites-dir
                                   (cffi:callback process-sprite-file)
                                   (cffi:null-pointer)))
      (al:destroy-fs-entry sprites-dir))))
