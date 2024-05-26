(in-package #:prejam-2024)


(ecs:defsystem test-win
  (:components-ro (map)
   :when (length= 0 (team 0)))
  (make-sound-effect -1 :win
                     (/ +window-width+ 2.0)
                     (/ +window-height+ 2.0)
                     :variations 1)
  (ecs:delete-entity entity)
  (setf *current-map* -1)
  (toggle-ui-window :win-message :on t))

(ecs:defsystem test-defeat
  (:components-ro (map)
    :when (length= 0 (team 1)))
  (make-sound-effect -1 :lose
                     (/ +window-width+ 2.0)
                     (/ +window-height+ 2.0)
                     :variations 1)
  (ecs:delete-entity entity)
  (setf *current-map* -1)
  (toggle-ui-window :loose-message :on t))
