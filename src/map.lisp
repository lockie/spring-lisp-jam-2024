(in-package #:cycle-of-evil)


(ecs:defcomponent map-tile
  (movement-cost 0.0 :type single-float)
  (grass 0 :type bit :index grass)
  (castle 0 :type bit :index castle))

(ecs:defsystem render-map-tiles
  (:components-ro (position size image map-tile)
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (al:draw-scaled-bitmap image-bitmap
                         0.0 0.0
                         size-width size-height
                         position-x position-y
                         (* +scale-factor+ size-width)
                         (* +scale-factor+ size-height)
                         0)
  ;; (al:draw-rectangle position-x position-y
  ;;                    (+ position-x (* +scale-factor+ size-width))
  ;;                    (+ position-y (* +scale-factor+ size-height))
  ;;                    (al:map-rgb 0 0 0) 1)
  )

(declaim (type ecs:entity *current-map*))
(defparameter *current-map* -1)

(declaim (ftype (function (fixnum) single-float) total-map-tile-movement-cost))
(defun total-map-tile-movement-cost (tile-hash)
  (let ((sum 0.0))
    (declare (type single-float sum))
    (with-tiles tile-hash tile
      (when (has-map-tile-p tile)
        (incf sum (map-tile-movement-cost tile))))
    (max sum 0.0)))

(defconstant +max-movement-cost+ 1.0)

(declaim (ftype (function (float-coordinate float-coordinate) boolean)
                obstaclep))
(defun obstaclep (x y)
  "Takes tile position and returns T if there are obstacles in there."
  (>= (total-map-tile-movement-cost (encode-float-coordinates x y))
      +max-movement-cost+))

(defun read-file-into-string (pathname &key (buffer-size 4096))
  (with-open-stream (stream (al:make-character-stream pathname))
    (alexandria:read-stream-content-into-string
     stream :buffer-size buffer-size)))

(defun load-bitmap (filename)
  (al:ensure-loaded #'al:load-bitmap (namestring filename)))

(declaim (inline maybe-keyword))
(defun maybe-keyword (value)
  (if (and (stringp value)
           (> (length value) 1)
           (string= ":" value :end2 1))
      (make-keyword (string-upcase (subseq value 1)))
      value))

(declaim (inline maybe-boolean))
(defun maybe-boolean (value)
  "We use bit instead of boolean in components to save space."
  (cond
    ((eq value t)   1)
    ((eq value nil) 0)
    (t value)))

(defun properties->spec (properties)
  (loop :for component :being :the :hash-key
        :using (hash-value slots) :of properties
        :when (typep slots 'hash-table)
        :collect (list* (make-keyword (string-upcase component))
                        (loop :for name :being :the :hash-key
                              :using (hash-value value) :of slots
                              :nconcing (list
                                         (make-keyword (string-upcase name))
                                         (maybe-keyword
                                          (maybe-boolean value)))))))

(defun tile->spec (tile tile-width tile-height bitmap map-entity)
  (copy-list
   `((:parent :entity ,map-entity)
     ;; TODO : delete sub-bitmaps when no longer needed
     (:image :bitmap ,(al:create-sub-bitmap
                       bitmap
                       (tiled:tile-pixel-x tile)
                       (tiled:tile-pixel-y tile)
                       tile-width tile-height))
     (:size :width ,(float tile-width)
            :height ,(float tile-height)))))

(defun tiled-color->allegro (color)
  (when color
    `(al::r ,(/ (tiled:tiled-color-r color) 255.0)
      al::g ,(/ (tiled:tiled-color-g color) 255.0)
      al::b ,(/ (tiled:tiled-color-b color) 255.0)
      al::a ,(/ (tiled:tiled-color-a color) 255.0))))

(defun ensure-relative (pathname)
  (let ((dirs (rest (pathname-directory pathname))))
    (make-pathname :directory (when dirs
                                (list* :relative dirs))
                   :defaults pathname)))

(defun load-map (filename)
  (let* ((map (tiled:load-map
               filename
               (lambda (path &rest rest)
                 (apply #'read-file-into-string (ensure-relative path) rest))))
         (tile-width (tiled:map-tile-width map))
         (tile-height (tiled:map-tile-height map))
         (tilemap (make-hash-table))
         (map-entity (ecs:make-object
                      `((:map :tint
                              (tiled-color->allegro
                               (gethash "tint" (tiled:properties map))))))))
    (assert (and (= tile-width +tile-size+) (= tile-height +tile-size+)))
    (dolist (tileset (tiled:map-tilesets map))
      (let ((bitmap (load-bitmap
                     (ensure-relative
                      (tiled:image-source (tiled:tileset-image tileset))))))
        (dolist (tile (tiled:tileset-tiles tileset))
          (let* ((external-tile-spec
                   (when (typep tile 'tiled:tiled-tileset-tile)
                     (properties->spec (tiled:properties tile))))
                 (internal-tile-spec
                   (tile->spec tile tile-width tile-height bitmap map-entity))
                 (tile-spec (nconc internal-tile-spec external-tile-spec)))
            (setf (gethash tile tilemap)
                  (adjoin '(:map-tile) tile-spec :test #'eq :key #'first))))))
    (dolist (layer (tiled:map-layers map))
      (cond ((typep layer 'tiled:tile-layer)
             (dolist (cell (tiled:layer-cells layer))
               (ecs:make-object
                (append
                 (gethash (tiled:cell-tile cell) tilemap)
                 `((:position :x ,(* +scale-factor+ (tiled:cell-x cell))
                              :y ,(* +scale-factor+ (tiled:cell-y cell))))))))
            ((typep layer 'tiled:object-layer)
             (dolist (object (tiled:object-group-objects layer))
               (ecs:make-object
                (nconc
                 (properties->spec (tiled:properties object))
                 `((:parent :entity ,map-entity)
                   (:position
                    :x ,(* +scale-factor+
                           (+ (tiled:object-x object)
                              (* 0.5 (tiled:object-width object))))
                    :y ,(* +scale-factor+
                           (- (tiled:object-y object)
                              (* 0.5 (tiled:object-height object))))))))))))
    (values map-entity (tiled:map-width map) (tiled:map-height map))))
