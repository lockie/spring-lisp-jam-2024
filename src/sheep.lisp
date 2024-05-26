(in-package #:prejam-2024)


(ecs:defcomponent sheep
  (meat-points 0 :type fixnum)
  (sex 0 :type bit :index sheep-of-sex)
  (vision-range 0.0 :type single-float)
  (movement-speed 0.0 :type single-float)
  (feed-probability 0.0 :type single-float)
  (breed-probability 0.0 :type single-float))

(ecs:defsystem mortify-sheep
  (:components-ro (health sheep position)
   :components-rw (sprite)
   :when (not (plusp health-points)))
  "Polly Parrot, wake up. Polly."
  (make-sound-effect entity :slaughter position-x position-y :variations 1)
  (setf sprite-name :resources
        sprite-sequence-name :m-spawn)
  (delete-health entity)
  (when (has-behavior-p entity)
    (delete-behavior-tree
     (behavior-type entity)
     entity)
    (delete-behavior entity))
  (make-meat entity :points sheep-meat-points)
  (delete-sheep entity))

(defconstant +female+ 0)
(defconstant +male+ 1)

(declaim (inline male-p))
(defun male-p (sex)
  (= sex +male+))

(define-behavior-tree-node (pick-grass
                            :components-ro (position sheep)
                            :with (grass-bushes := (shuffle (grass 1))))
    ()
  "Picks the random grass bush as a target. Fails if there is no grass nearby."
  (flet ((sqr (x) (* x x)))
    (loop
      :for grass :of-type ecs:entity :in grass-bushes
      :for distance := (with-position (grass-x grass-y) grass
                         (distance* position-x position-y grass-x grass-y))
      :when (<= distance (sqr sheep-vision-range))
      :do (assign-target entity :entity grass)
          (return-from ecs::current-entity (complete-node t))
      :finally (complete-node nil))))

(define-constant +max-sheep-population+ 10)

(define-behavior-tree-node (pick-mate
                            :components-ro (sheep position)
                            :with (females :=
                                           (shuffle (sheep-of-sex +female+)))
                            :enable (< (length females) +max-sheep-population+))
    ()
  "Picks the random sheep to mate. Fails if there is no possible mate nearby."
  (if (male-p sheep-sex)
      (flet ((sqr (x) (* x x)))
        (loop
          :for female :of-type ecs:entity :in females
          :for distance := (with-position (female-x female-y) female
                             (distance* position-x position-y female-x female-y))
          :when (<= distance (sqr sheep-vision-range))
          :do (assign-target entity :entity female)
              (return-from ecs::current-entity (complete-node t))
          :finally (complete-node nil)))
      (complete-node nil)))

(define-behavior-tree-node (hump
                            :components-ro (position)
                            :components-rw (target sprite animation-state)
                            :arguments ((:dt single-float)))
    ((time 2.0 :type single-float :documentation "Hump time in seconds."))
  "A little bit of Netflix & chill."
  (cond ((plusp hump-time)
         (when (= hump-time 2.0)
           (make-sound-effect entity :sheep position-x position-y
                              :variations 1))
         (setf sprite-sequence-name :hump
               animation-state-flip (animation-state-flip target-entity))
         (decf hump-time dt))
        (t
         (setf sprite-sequence-name :idle)
         (complete-node t))))

(define-behavior-tree-node (reproduce
                            :components-ro (position health sheep target))
    ()
  "Sheep birth."
  (let+ ((mom target-entity)
         ((&flet average (a b)
            (* 0.5 (+ a b))))
         ((&macrolet inherit (feature)
            `(random (+ 1 ,feature (,feature mom))))))
    (with-position (mom-x mom-y) mom
      (ecs:make-object
       `((:parent :entity ,entity)
         (:position :x ,(average position-x mom-x)
                    :y ,(average position-y mom-y))
         (:health :points ,(inherit health-points))
         (:sheep
          :meat-points ,(inherit sheep-meat-points)
          :sex ,(random 2)
          :vision-range ,(inherit sheep-vision-range)
          :movement-speed ,(inherit sheep-movement-speed)
          :feed-probability ,(clamp (inherit sheep-feed-probability) 0.0 1.0)
          :breed-probability ,(clamp (inherit sheep-breed-probability) 0.0 1.0))
         (:behavior :type :sheep)
         (:sprite :name :happy-sheep :sequence-name :idle)))))
  (complete-node t))

(define-behavior-tree sheep
    ((repeat :name "root")
     ((fallback)
      ((sequence :name "feed")
       ((random :probability (sheep-feed-probability entity)))
       ((pick-grass))
       ((repeat-until-fail)
        ((sequence :name "browse")
         ((invert)
          ((test-target-near :range +scaled-tile-size+)))
         ((calculate-path))
         ((follow-path))
         ((move :speed (sheep-movement-speed entity)))))
       ((wait :name "munch" :time 2.0)))
      ((sequence :name "breed")
       ((random :probability (sheep-breed-probability entity)))
       ((pick-mate))
       ((repeat-until-fail)
        ((sequence :name "animal-sex")
         ((repeat-until-fail)
          ((sequence :name "pursuit")
           ((test-target-alive))
           ((invert)
            ((test-target-near :range +scaled-tile-size+)))
           ((calculate-path))
           ((follow-path))
           ((move :speed (sheep-movement-speed entity)))))
         ((hump))
         ((test-target-alive))
         ((test-target-near :range +scaled-tile-size+))
         ((invert)
          ((reproduce))))))
      ((idle))
      ((wait :time 1.0)))))
