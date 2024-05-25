(in-package #:prejam-2024)


(define-constant +repl-update-interval+ 0.3d0)

(define-constant +fps-font-path+ "../Resources/fonts/inconsolata.ttf"
  :test #'string=)
(define-constant +fps-font-size+ 24)
(define-constant +damage-numbers-font-path+ "../Resources/fonts/acme.ttf"
  :test #'string=)
(define-constant +damage-numbers-font-size+ 26)
(define-constant +ui-font-path+ "../Resources/fonts/acme.ttf"
  :test #'string=)
(define-constant +ui-font-size+ 28)

(define-constant +config-path+ "../config.cfg"
  :test #'string=)

(defun init ()
  (ecs:bind-storage)
  (load-ui)
  (load-sprites)
  (load-sounds)
  (let+ (((&values map width height) (load-map "/test2.tmx")))
    (setf *current-map* map
          *world-width* width
          *world-height* height))
  (trivial-garbage:gc :full t))

(declaim (type fixnum *fps*))
(defvar *fps* 0)

(defun update (dt ui-context)
  (unless (zerop dt)
    (setf *fps* (round 1 dt)))
  (ecs:run-systems :dt (float dt 0.0) :ui-context ui-context))

(define-constant +white+ (al:map-rgba 255 255 255 0) :test #'equalp)

(defun render (fps-font)
  (nk:allegro-render)
  (when (al:get-config-value (al:get-system-config) "trace" "fps")
    (al:draw-text fps-font +white+ 0 0 0 (format nil "~d FPS" *fps*))))

(declaim (inline pressed-key))
(defun pressed-key (event)
  (cffi:with-foreign-slots ((al::keycode) event (:struct al:keyboard-event))
    al::keycode))

(cffi:defcallback %main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (handler-bind
      ((error #'(lambda (e) (unless *debugger-hook*
                         (al:show-native-message-box
                          (cffi:null-pointer) "Hey guys"
                          "We got a big error here :("
                          (with-output-to-string (s)
                            (uiop:print-condition-backtrace e :stream s))
                          (cffi:null-pointer) :error)))))
    (al:set-app-name "prejam-2024")
    (unless (al:init)
      (error "Initializing liballegro failed"))
    (let ((config (al:load-config-file +config-path+)))
      (unless (cffi:null-pointer-p config)
        (al:merge-config-into (al:get-system-config) config)))
    (al:set-config-value (al:get-system-config) "trace" "level" "debug")
    (unless (al:init-primitives-addon)
      (error "Initializing primitives addon failed"))
    (unless (al:init-image-addon)
      (error "Initializing image addon failed"))
    (unless (al:init-font-addon)
      (error "Initializing liballegro font addon failed"))
    (unless (al:init-ttf-addon)
      (error "Initializing liballegro TTF addon failed"))
    (unless (al:install-audio)
      (error "Intializing audio addon failed"))
    (unless (al:init-acodec-addon)
      (error "Initializing audio codec addon failed"))
    (unless (al:restore-default-mixer)
      (error "Initializing default audio mixer failed"))
    (al:set-new-display-option :vsync 2 :require)
    (let ((display (al:create-display +window-width+ +window-height+))
          (event-queue (al:create-event-queue)))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (al:inhibit-screensaver t)
      (al:set-window-title display "Prejam 2024")
      (al:register-event-source event-queue
                                (al:get-display-event-source display))
      (al:install-keyboard)
      (al:register-event-source event-queue
                                (al:get-keyboard-event-source))
      (al:install-mouse)
      (al:register-event-source event-queue
                                (al:get-mouse-event-source))
      (al:set-new-bitmap-flags '(:min-linear))
      (unwind-protect
           (cffi:with-foreign-object (event '(:union al:event))
             (init)
             (#+darwin trivial-main-thread:call-in-main-thread #-darwin funcall
              #'livesupport:setup-lisp-repl)
             (loop
               :named main-game-loop
               :with *should-quit* := nil
               :with fps-font := (al:ensure-loaded #'al:load-ttf-font
                                                   +fps-font-path+
                                                   (- +fps-font-size+) 0)
               :with *damage-numbers-font* := (al:ensure-loaded
                                               #'al:load-ttf-font
                                               +damage-numbers-font-path+
                                               (- +damage-numbers-font-size+) 0)
               :with ui-font := (al:ensure-loaded
                                 #'nk:allegro-font-create-from-file
                                 +ui-font-path+ (- +ui-font-size+) 0)
               :with ui-context := (nk:allegro-init ui-font display
                                                    +window-width+
                                                    +window-height+)
               :with ticks :of-type double-float := (al:get-time)
               :with last-repl-update :of-type double-float := ticks
               :with dt :of-type double-float := 0d0
               :while (loop
                        :named event-loop
                        :initially (nk:input-begin ui-context)
                        :while (al:get-next-event event-queue event)
                        :for type := (cffi:foreign-slot-value
                                      event '(:union al:event) 'al::type)
                        :do (if (and (eq type :key-down)
                                     (eq (pressed-key event) :escape))
                                (toggle-ui-window :main-menu)
                                (nk:allegro-handle-event event))
                        :always (not (eq type :display-close))
                        :finally (nk:input-end ui-context))
               :never *should-quit* ;; lol
               :do (let ((new-ticks (al:get-time)))
                     (setf dt (- new-ticks ticks)
                           ticks new-ticks))
                   (when (> (- ticks last-repl-update)
                            +repl-update-interval+)
                     (livesupport:update-repl-link)
                     (setf last-repl-update ticks))
                   (al:clear-to-color +black+)
                   (livesupport:continuable
                     (update dt ui-context)
                     (render fps-font))
                   (al:flip-display)
               :finally (nk:allegro-shutdown)
                        (nk:allegro-font-del ui-font)
                        (al:destroy-font *damage-numbers-font*)
                        (al:destroy-font fps-font)))
        (al:inhibit-screensaver nil)
        (al:destroy-event-queue event-queue)
        (al:destroy-display display)
        (al:stop-samples)
        (al:uninstall-system)
        (al:uninstall-audio)
        (al:shutdown-ttf-addon)
        (al:shutdown-font-addon)
        (al:shutdown-image-addon))))
  0)

(defun main ()
  (#+darwin trivial-main-thread:with-body-in-main-thread #-darwin progn nil
    (float-features:with-float-traps-masked
        (:divide-by-zero :invalid :inexact :overflow :underflow)
      (al:run-main 0 (cffi:null-pointer) (cffi:callback %main)))))

