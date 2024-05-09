(in-package #:prejam-2024)


(ecs:defcomponent map-tile
  (movement-cost 0 :type fixnum))

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
                         0))

(defun read-file-into-string (pathname &key (buffer-size 4096))
  (with-open-stream (stream (al:make-character-stream pathname))
    (alexandria:read-stream-content-into-string
     stream :buffer-size buffer-size)))

(defun load-bitmap (filename)
  (al:ensure-loaded #'al:load-bitmap (namestring filename)))

(defun properties->spec (properties)
  (loop :for component :being :the :hash-key
        :using (hash-value slots) :of properties
        :when (typep slots 'hash-table)
        :collect (list* (make-keyword (string-upcase component))
                        (loop :for name :being :the :hash-key
                              :using (hash-value value) :of slots
                              :nconcing (list
                                         (make-keyword (string-upcase name))
                                         value)))))

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

(defun load-map (filename)
  (let* ((map (tiled:load-map
               filename
               (lambda (path &rest rest)
                 (apply #'read-file-into-string (resource-path path) rest))))
         (tile-width (tiled:map-tile-width map))
         (tile-height (tiled:map-tile-height map))
         (tilemap (make-hash-table))
         (map-entity (ecs:make-entity)))
    (assert (and (= tile-width +tile-size+) (= tile-height +tile-size+)))
    (dolist (tileset (tiled:map-tilesets map))
      (let ((bitmap (load-bitmap
                     (resource-path
                      (tiled:image-source (tiled:tileset-image tileset))))))
        (dolist (tile (tiled:tileset-tiles tileset))
          (let* ((external-tile-spec
                   (when (typep tile 'tiled:tiled-tileset-tile)
                     (properties->spec (tiled:properties tile))))
                 (internal-tile-spec
                   (tile->spec tile tile-width tile-height bitmap map-entity))
                 (tile-spec (append internal-tile-spec external-tile-spec)))
            (unless (assoc :map-tile external-tile-spec)
              (setf tile-spec (nconc tile-spec '((:map-tile)))))
            (setf (gethash tile tilemap) tile-spec)))))
    (dolist (layer (tiled:map-layers map))
      (cond ((typep layer 'tiled:tile-layer)
             (dolist (cell (tiled:layer-cells layer))
               (let* ((tile-spec (gethash (tiled:cell-tile cell) tilemap))
                      (tile-instance (ecs:make-object tile-spec)))
                 (make-position tile-instance
                                :x (* +scale-factor+ (tiled:cell-x cell))
                                :y (* +scale-factor+ (tiled:cell-y cell))))))
            ((typep layer 'tiled:object-layer)
             (dolist (object (tiled:object-group-objects layer))
               (let ((entity (ecs:make-object
                              (properties->spec (tiled:properties object)))))
                 (make-parent entity :entity map-entity)
                 (make-position
                  entity :x (* +scale-factor+ (tiled:object-x object))
                         :y (* +scale-factor+ (tiled:object-y object))))))))
    (values map-entity (tiled:map-width map) (tiled:map-height map))))
