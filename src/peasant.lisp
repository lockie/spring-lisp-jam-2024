(in-package #:prejam-2024)


(ecs:defcomponent peasant
  (vision-range 0.0 :type single-float)
  (movement-speed 0.0 :type single-float)
  (slaughter-range 0.0 :type single-float)
  (slaughter-damage 0 :type fixnum)
  (slaughter-cooldown 0.0 :type single-float)
  (unload-range 0.0 :type single-float)
  (eagerness 0.5 :type single-float
                 :documentation "Probability to not to slack off, 0 to 1")
  (poison-probability 0.0 :type single-float
                          :documentation "Probability to poision the meat")
  (poison-dps 0 :type fixnum)
  (rest-time 5.0 :type single-float))

(ecs:defsystem carry-meat
  (:components-ro (meat)
   :components-rw (position)
   :when (ecs:entity-valid-p meat-carry))
  (with-position (carry-x carry-y) meat-carry
    (setf position-x carry-x
          position-y (- carry-y (/ +scaled-tile-size+ 2)))))

(define-behavior-tree-node (pick-sheep
                            :components-ro (position peasant)
                            :with (livestock := (shuffle
                                                 (sheep-of-sex
                                                  (random 2)))))
    ()
  "Picks the random sheep as a target. Fails if there is no sheep nearby."
  (flet ((sqr (x) (* x x)))
    (loop
      :for sheep :of-type ecs:entity :in livestock
      :for distance := (with-position (sheep-x sheep-y) sheep
                         (distance* position-x position-y sheep-x sheep-y))
      :when (<= distance (sqr peasant-vision-range))
        :do (assign-target entity :entity sheep)
            (return-from ecs::current-entity (complete-node t))
      :finally (complete-node nil))))

(define-behavior-tree-node (butcher
                            :components-ro (position peasant target
                                                     animation-sequence)
                            :components-rw (sprite animation-state))
    ((started 0 :type bit)
     (done 0 :type bit
             :documentation "Attack happened, but animation's still playing"))
  (with-position (target-x target-y) target-entity
    (cond ((zerop butcher-started)
           (setf sprite-sequence-name :build
                 animation-state-flip (if (minusp (- target-x position-x)) 1 0)
                 butcher-started 1))

          ((and (zerop butcher-done)
                (= animation-state-frame (- animation-sequence-frames 3)))
           (make-damage target-entity peasant-slaughter-damage)
           (make-sound-effect entity :punch position-x position-y)
           (setf butcher-done 1))

          ((and (plusp animation-state-finished)
                (= animation-state-frame (1- animation-sequence-frames)))
           (setf sprite-sequence-name :idle)
           (complete-node t)))))

(define-behavior-tree-node (poison-meat
                            :components-ro (target peasant))
    ()
  (make-poisoned target-entity :dps peasant-poison-dps)
  (complete-node t))

(define-behavior-tree-node (pick-up
                            :components-ro (peasant target)
                            :components-rw (sprite))
    ()
  (with-meat () target-entity
    (if (ecs:entity-valid-p carry)
        (complete-node nil) ;; someone else took it
        (complete-node
         (setf sprite-sequence-name :carry
               carry entity)))))

(define-behavior-tree-node (locate-drop-off
                            :components-ro (position peasant)
                            :with (castles := (castle 1)))
    ()
  (assign-target entity :entity (random-elt castles))
  (complete-node t))

(define-behavior-tree-node (unload
                            :components-ro (peasant target)
                            :components-rw (sprite))
    ()
  (when-let (meat (first (meat-carried-by entity :count 1)))
    (setf (meat-carry meat) -1))
  (setf sprite-sequence-name :idle)
  (complete-node t))

;; "More work?"
(define-behavior-tree peasant
    ((repeat :name "root")
     ((fallback)
      ((sequence)
       ((random :probability (peasant-eagerness entity)))
       ((pick-sheep))
       ((invert)
        ((repeat-until-fail)
         ((sequence :name "slaughter")
          ((sequence)
           ((repeat-until-fail)
            ((sequence :name "pursuit")
             ((test-target-alive))
             ((invert)
              ((test-target-near :range (peasant-slaughter-range entity))))
             ((calculate-path))
             ((follow-path))
             ((move :speed (peasant-movement-speed entity)))))
           ((test-target-near :range (peasant-slaughter-range entity))))
          ((repeat-until-fail)
           ((sequence :name "attacks")
            ((test-target-alive))
            ((test-target-near :range (peasant-slaughter-range entity)))
            ((butcher))
            ((wait :time (peasant-slaughter-cooldown entity)))))
          ((fallback)
           ((test-target-alive))
           ((sequence)
            ((fallback)
             ((sequence :name "poision")
              ((random :probability (peasant-poison-probability entity)))
              ((poison-meat)))
             ((always-true)))
            ((pick-up))
            ((locate-drop-off))
            ((invert)
             ((repeat-until-fail)
              ((sequence :name "carry")
               ((invert)
                ((test-target-near :range (peasant-unload-range entity))))
               ((calculate-path))
               ((follow-path))
               ((move :speed (peasant-movement-speed entity)
                      :animation-sequence :run-carry))))))
           ((invert)
            ((unload))))))))
      ((invert)
       ((idle)))
      ((invert)
       ((sequence :name "wander-off")
        ((wander-off))
        ((move :speed (peasant-movement-speed entity)))
        ((idle))))
      ((sequence :name "slack-off")
       ((wait :time (peasant-rest-time entity)))))))
