(in-package #:cycle-of-evil)


(ecs:defcomponent ui-window
  (name :|| :type keyword :index ui-window :unique t)
  (function 'identity :type symbol)
  (shown 0 :type bit))

(ecs:defsystem render-ui-windows
  (:components-ro (ui-window)
   :when (plusp ui-window-shown)
   :arguments ((:ui-context cffi:foreign-pointer)))
  (funcall ui-window-function ui-context))

(defun toggle-ui-window (name &key (on nil value-supplied-p))
  (with-ui-window () (ui-window name)
    (setf shown
          (if value-supplied-p
              (if on 1 0)
              (logxor shown 1)))))

(defmacro defwindow (name options &body body)
  (with-gensyms ((window-name name))
    (let ((keyword (make-keyword name)))
      `(progn
         (let ((,window-name (ui-window ,keyword :missing-error-p nil)))
           ;; allow hot reload
           (when (ecs:entity-valid-p ,window-name)
             (ecs:delete-entity ,window-name)))
         (ecs:make-object
          `((:ui-window
             :name ,,keyword
             :function ,(ui:defwindow ,name () ,options ,@body))))))))

(defun load-ui-image (name)
  (al:ensure-loaded #'nk:allegro-create-image name))

(declaim (type boolean *should-quit*))
(defvar *should-quit* nil)

(define-modify-macro mulf (factor) *)

(defparameter *options* (make-hash-table :size 4))
(defparameter *option-names*
  (vector "Purity" "Sabotage" "Shaman" "Miser"))
(defparameter *option-descriptions*
  (vector
   "So it was."
   "We burned down their forge. Their blacksmith hangs from a tree. Does anyone among them still know which side of the hammer to hold?"
   "Oh, what blue fairies. If I wave my hands just as quickly, I will be able to fly alongside them."
   "I know who they buy weapons from. If we sell them some of our weapons, we can be sure that the blades will have a defect."))
(defparameter *option-processors*
  (vector
   (lambda ())
   (lambda ()
     (dolist (character (team 0))
       (setf (character-defense-multiplier character) 2.0)))
   (lambda ()
     (dolist (character (team 1))
       (mulf (character-attack-cooldown character) 0.5)
       (mulf (character-projectile-speed character) 2.0)))
   (lambda ()
     (dolist (character (team 0))
       (setf (character-damage-min character)
             (round (* 0.5 (character-damage-min character)))
             (character-damage-max character)
             (round (* 0.5 (character-damage-max character)))))
     (dolist (character (team 1))
       (setf (character-damage-min character)
             (round (* 0.7 (character-damage-min character)))
             (character-damage-max character)
             (round (* 0.7 (character-damage-max character))))))))

(defparameter *selected-map* 0)
(defparameter *selected-option* 0)

(declaim (inline find-max-key))
(defun find-max-key (hash-table)
  (let ((max-key 0)
        (max-val most-negative-fixnum))
    (declare (type fixnum max-val))
    (maphash (lambda (key val)
               (when (> val max-val)
                 (setf max-val val
                       max-key key)))
             hash-table)
    max-key))

(defparameter *outcome-descriptions*
  (vector
   "You're lying, it can't be true. You're a cheater."
   "The last of the knights has fallen. We passed through fire across the world. No one escaped our righteous wrath. And only ashes remain everywhere... But we also depart, and the land yields no more fruit. This is an endless winter."
   "The nation's color vanished in the mad haste of potions. The knights will follow suit, but what do we care for them? Only the elders watch the dying embers of the plague that followed, the world erases us."
   "The dealers deceived us all... But at least we survived. Just like the knights. Perhaps there is a higher meaning in this? All this bloodshed wasn't worth it, but it's good that our mistake prevented the worst. We may not be able to accept the knights for a long time yet, but peace is possible, and maybe we won't destroy everything around us."))

(defun load-ui ()
  (let ((win-background (load-ui-image "images/ui/ribbon_red_3slides.png")))
    (defwindow win-message
        (:x 544 :y 368 :w 192 :h 64
         :flags (:no-scrollbar)
         :styles ((:item-color :window-fixed-background :a 0)
                  (:item-image :button-normal win-background)
                  (:item-image :button-hover win-background)
                  (:item-image :button-active win-background)))
      (toggle-ui-window :main-menu :on nil)
      (ui:layout-space (:format :dynamic :height 64 :widget-count 1)
        (ui:layout-space-push :x 0 :y 0 :w 1 :h 1)
        (ui:button-label "You won!"
          (when (= *selected-map* *current-progress*)
            (incf *current-progress*)
            (incf (gethash *selected-option* *options* 0)))
          (setf *selected-map* *current-progress*
                *selected-option* 0)
          (toggle-ui-window :win-message :on nil)
          (if (= *current-progress* (/ (length *map-descriptions*) 2))
              (toggle-ui-window :outcome :on t)
              (toggle-ui-window :map-selector :on t))))))

  (let ((loose-background (load-ui-image "images/ui/ribbon_blue_3slides.png")))
    (defwindow loose-message
        (:x 544 :y 368 :w 192 :h 64
         :flags (:no-scrollbar)
         :styles ((:item-color :window-fixed-background :a 0)
                  (:item-image :button-normal loose-background)
                  (:item-image :button-hover loose-background)
                  (:item-image :button-active loose-background)))
      (toggle-ui-window :main-menu :on nil)
      (ui:layout-space (:format :dynamic :height 64 :widget-count 1)
        (ui:layout-space-push :x 0 :y 0 :w 1 :h 1)
        (ui:button-label "You've lost!"
          (setf *selected-option* 0)
          (toggle-ui-window :loose-message :on nil)
          (toggle-ui-window :map-selector :on t)))))

  (let+ ((backgrounds
          (vector
           (load-ui-image "images/outcomes/0.png")
           (load-ui-image "images/outcomes/1.png")
           (load-ui-image "images/outcomes/2.png")
           (load-ui-image "images/outcomes/3.png")))
         ((&flet background ()
            (aref backgrounds (find-max-key *options*))))
         ((&flet description ()
            (aref *outcome-descriptions* (find-max-key *options*)))))
    (defwindow outcome
        (:x 0 :y 0 :w +window-width+ :h +window-height+
         :flags (:no-scrollbar)
         :styles ((:item-image :window-fixed-background (background))
                  (:color :text-color :r 0 :g 0 :b 0)))
      (toggle-ui-window :main-menu :on nil)
      (ui:layout-space (:format :dynamic :height 110 :widget-count 1)
        (ui:layout-space-push :x 0.15 :y 0.05 :w 0.7 :h 1.0)
        (ui:label-wrap (description))
        (ui:layout-space-push :x 0.64 :y 0 :w 0.20 :h 0.35)
        (ui:button-label "Maybe there's another way"
          (setf *options* (make-hash-table :size 4)
                *selected-map* 0
                *current-progress* 0)
          (toggle-ui-window :outcome :on nil)
          (toggle-ui-window :map-selector :on t)))))

  (let ((button-normal
          (load-ui-image "images/ui/button_red_3slides.png"))
        (button-pressed
          (load-ui-image "images/ui/button_red_3slides_pressed.png"))
        (button-disabled
          (load-ui-image "images/ui/button_disable_3slides.png")))
    (defwindow main-menu
        (:x 0 :y 0 :w +window-width+ :h +window-height+
         :styles ((:item-color :window-fixed-background :a 192)
                  (:item-image :button-normal button-normal)
                  (:item-image :button-hover button-normal)
                  (:item-image :button-active button-pressed)
                  (:color :button-text-normal :r 86 :g 83 :b 97)
                  (:color :button-text-hover :r 22 :g 28 :b 46)
                  (:color :button-text-active :r 22 :g 28 :b 46)))
      (ui:layout-space (:format :dynamic :height 64 :widget-count 1)
        (ui:layout-space-push :x 0.425 :y 4 :w 0.15 :h 0.8)
        (let ((has-map-p (ecs:entity-valid-p *current-map*)))
          (ui:styles ((:item-image :button-normal (if has-map-p
                                                      button-normal
                                                      button-disabled))
                      (:item-image :button-hover (if has-map-p
                                                     button-normal
                                                     button-disabled))
                      (:item-image :button-active (if has-map-p
                                                      button-pressed
                                                      button-disabled))
                      (:color :button-text-hover
                       :r (if has-map-p 22 86)
                       :g (if has-map-p 28 83)
                       :b (if has-map-p 46 97))
                      (:color :button-text-active
                       :r (if has-map-p 22 86)
                       :g (if has-map-p 28 83)
                       :b (if has-map-p 46 97)))
            (ui:button-label "Continue"
              (when has-map-p
                (toggle-ui-window :main-menu :on nil)))
            (ui:button-label "Abandon battle"
              (when has-map-p
                (ecs:delete-entity *current-map*)
                (setf *current-map* -1
                      *selected-option* 0)
                (toggle-ui-window :main-menu :on nil)
                (toggle-ui-window :map-selector :on t)))))
        (ui:button-label "RAGEQUIT!"
          (setf *should-quit* t)))))

  (let ((maps-background
          (load-ui-image "images/ui/map.png"))
        (button-normal
          (load-ui-image "images/ui/button_blue_3slides.png"))
        (button-pressed
          (load-ui-image "images/ui/button_blue_3slides_pressed.png")))
    (defwindow map-selector
        (:x 0 :y 0 :w +window-width+ :h +window-height+
         :flags (:no-scrollbar)
         :styles ((:item-image :window-fixed-background maps-background)))
      (ui:with-context context
        (ui:layout-space (:format :dynamic
                          :height +window-height+ :widget-count 3)
          (ui:layout-space-push :x 0.06 :y 0.06 :w 0.5 :h 0.9)
          (ui:defgroup battles
              (:flags (:no-scrollbar :title)
               :styles ((:item-color :window-fixed-background :a 190)
                        (:item-color :window-header-normal :a 190)
                        (:item-color :window-header-hover :a 190)
                        (:item-color :window-header-active :a 190)))
            (loop :for (name description) :on *map-descriptions* :by #'cddr
                  :for i :of-type fixnum :from 0
                  :do (ui:layout-row-static :height 30 :item-width 780
                                            :columns 1)
                      (when (plusp
                             (nk:option-label context name
                                              (if (= i *selected-map*) 1 0)))
                        (setf *selected-map* i))
                      (ui:layout-row-dynamic :height 100 :columns 1)
                      (ui:label-wrap description)
                      (when (= i *current-progress*)
                        (nk:widget-disable-begin context))
                  :finally (nk:widget-disable-end context)))
          (ui:layout-space-push :x 0.58 :y 0.06 :w 0.38 :h 0.7)
          (ui:defgroup options
              (:flags (:no-scrollbar)
               :styles ((:item-color :window-fixed-background :a 190)))
            (loop :for i :of-type fixnum :from 0 :below 4
                  :do (ui:layout-row-static :height 30 :item-width 350
                                            :columns 1)
                      (when (plusp
                             (nk:option-label context (aref *option-names* i)
                                              (if (= i *selected-option*) 1 0)))
                        (setf *selected-option* i))
                      (ui:layout-row-dynamic :height 100 :columns 1)
                      (ui:label-wrap (aref *option-descriptions* i))))
          (ui:layout-space-push :x 0.58 :y 0.80 :w 0.38 :h 0.158)
          (ui:defgroup button
              (:flags (:no-scrollbar)
               :styles ((:item-color :window-fixed-background :a 190)
                        (:item-image :button-normal button-normal)
                        (:item-image :button-hover button-normal)
                        (:item-image :button-active button-pressed)
                        (:color :button-text-normal :r 86 :g 83 :b 97)
                        (:color :button-text-hover :r 22 :g 28 :b 46)
                        (:color :button-text-active :r 22 :g 28 :b 46)))
            (ui:layout-space (:format :dynamic :height 64 :widget-count 1)
              (ui:layout-space-push :x 0.31 :y 0.5 :w 0.39 :h 0.75)
              (ui:button-label "To arms!"
                (let+ (((&values map width height)
                        (load-map (format nil "/~a.tmx" *selected-map*))))
                  (funcall (aref *option-processors* *selected-option*))
                  (toggle-ui-window :map-selector :on nil)
                  (setf *current-map* map
                        *world-width* width
                        *world-height* height)))))))))
  nil)
