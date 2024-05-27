(in-package #:cycle-of-evil)


(ecs:defcomponent sound-prefab
  (name :|| :type keyword :documentation "Sound name"
        :index sound-prefab :unique t)
  (sample (cffi:null-pointer) :type cffi:foreign-pointer))

(ecs:defcomponent sound
  (sample-instance (cffi:null-pointer) :type cffi:foreign-pointer)
  (repeat 0 :type bit)
  (time 0.0 :type single-float))

(ecs:defsystem expire-sound
  (:components-rw (sound)
   :arguments ((:dt single-float)))
  (decf sound-time dt)
  (when (and (not (plusp sound-time))
             (zerop sound-repeat))
    (ecs:delete-entity entity)))

(ecs:hook-up ecs:*entity-deleting-hook*
             (lambda (entity)
               (when (has-sound-p entity)
                 (let ((sample-instance (sound-sample-instance entity)))
                   (al:stop-sample-instance sample-instance)
                   (al:destroy-sample-instance sample-instance))
                 (setf (sound-sample-instance entity) (cffi:null-pointer)))))

(defconstant +distance-factor+
  ;; NOTE this math is very experimental math
  (/ 4.0 (sqrt (+ (* +window-width+ +window-width+)
                  (* +window-height+ +window-height+)))))

(defconstant +pan-factor+ (/ 2.0 +window-width+))

(declaim (inline position-sound)
         (ftype (function (cffi:foreign-pointer single-float single-float))
                position-sound))
(defun position-sound (sample-instance x y)
  (let ((gain (/ 1.0 (exp (* +distance-factor+
                              (distance x y
                                        (/ +window-width+ 2.0)
                                        (/ +window-height+ 2.0))))))
         (pan (clamp (* +pan-factor+ (- x (/ +window-width+ 2.0))) -1.0 1.0)))
    (al:set-sample-instance-gain sample-instance gain)
    (al:set-sample-instance-pan sample-instance pan)))

(ecs:defsystem position-sound
  (:components-ro (sound parent)
   :components-rw (position)
   :when (ecs:entity-valid-p parent-entity))
  (with-position (parent-x parent-y) parent-entity
    (when (or (/= position-x parent-x)
              (/= position-y parent-y))
      (position-sound sound-sample-instance
                      (setf position-x parent-x)
                      (setf position-y parent-y))
      (setf position-tile (tile-hash position-x position-y)))))

(defun make-sound-effect (parent name x y &key repeat (variations 4))
  (let* ((name (format-symbol :keyword "~a~a" name (random variations)))
         (prefab (sound-prefab name))
         (sample (sound-prefab-sample prefab))
         (sample-instance (al:create-sample-instance sample))
         (time (al:get-sample-instance-time sample-instance)))
    (al:attach-sample-instance-to-mixer sample-instance (al:get-default-mixer))
    (position-sound sample-instance x y)
    (al:set-sample-instance-playmode sample-instance (if repeat :loop :once))
    (al:play-sample-instance sample-instance)
    (ecs:make-object
     `((:parent :entity ,parent)
       (:position :x ,x :y ,y)
       (:sound :sample-instance ,sample-instance
               :repeat ,(if repeat 1 0)
               :time ,time)))))

(defun load-sound-file (full-filename)
  (let ((sample (al:ensure-loaded #'al:load-sample full-filename))
        (name (make-keyword (string-upcase (kebabize
                                            (pathname-name full-filename))))))
    (ecs:make-object
     `((:sound-prefab :name ,name :sample ,sample)))))

(defun load-sound (filename)
  (load-sound-file (namestring (resource-path filename))))

(cffi:defcallback process-sound-file :int
    ((file (:pointer (:struct al::fs-entry))) (data :pointer))
  (declare (ignore data))
  (if (zerop (logand (al::get-fs-entry-mode file)
                     (cffi:foreign-bitfield-value 'al::file-mode '(:isdir))))
      (progn
        (load-sound-file (al::get-fs-entry-name file))
        (cffi:foreign-enum-value 'al::for-each-fs-entry-result :ok))
      (cffi:foreign-enum-value 'al::for-each-fs-entry-result :skip)))

(defun load-sounds ()
  (let ((sounds-dir (al:create-fs-entry (namestring (resource-path "sounds")))))
    (unwind-protect
         (= (cffi:foreign-enum-value 'al::for-each-fs-entry-result :ok)
            (al::for-each-fs-entry sounds-dir
                                   (cffi:callback process-sound-file)
                                   (cffi:null-pointer)))
      (al:destroy-fs-entry sounds-dir))))
