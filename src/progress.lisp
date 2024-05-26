(in-package #:prejam-2024)


(ecs:defsystem test-win
  (:components-ro (map)
   :when (length= 0 (team 0)))
  (ecs:delete-entity entity)
  (toggle-ui-window :win-message :on t))

(ecs:defsystem test-defeat
  (:components-ro (map)
    :when (length= 0 (team 1)))
  (ecs:delete-entity entity)
  (toggle-ui-window :loose-message :on t))
