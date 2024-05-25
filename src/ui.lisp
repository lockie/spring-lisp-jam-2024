(in-package #:prejam-2024)


(ecs:defcomponent ui-window
  (name :|| :type keyword :index ui-window :unique t)
  (function #'identity :type function)
  (shown 0 :type bit))

(ecs:defsystem render-ui-windows
  (:components-ro (ui-window)
   :when (plusp ui-window-shown)
   :arguments ((:ui-context cffi:foreign-pointer)))
  (funcall ui-window-function ui-context))

(defun toggle-ui-window (name)
  (with-ui-window () (ui-window name)
    (setf shown (logxor shown 1))))

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
  (al:ensure-loaded #'nk:allegro-create-image (namestring
                                               (resource-path name))))

(declaim (type boolean *should-quit*))
(defvar *should-quit* nil)

(defun load-ui ()
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
                (toggle-ui-window :main-menu)))
            (ui:button-label "Abandon castle"
              (when has-map-p
                (ecs:delete-entity *current-map*)
                (setf *current-map* -1)))))
        (ui:button-label "RAGEQUIT!"
          (setf *should-quit* t)))))
  nil)

