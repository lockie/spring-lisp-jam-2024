(in-package #:cycle-of-evil)


(ecs:defsystem render-map-tint
  (:components-ro (map)
   :when map-tint)
  (al:draw-filled-rectangle 0 0 +window-width+ +window-height+ map-tint))
