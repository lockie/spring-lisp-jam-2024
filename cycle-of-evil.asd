(defsystem "cycle-of-evil"
  :version "0.0.1"
  :author "Andrew Kravchuk"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-aseprite
               #:cl-fast-behavior-trees
               #:cl-fast-ecs
               #:cl-liballegro
               #:cl-liballegro-nuklear
               #:cl-liballegro-nuklear/declarative
               #:cl-tiled
               #:global-vars
               #:let-plus
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "common")
                 (:file "sound")
                 (:file "damage")
                 (:file "sprite")
                 (:file "tint")
                 (:file "map")
                 (:file "poison")
                 (:file "fire")
                 (:file "projectile")
                 (:file "priority-queue")
                 (:file "character")
                 (:file "offensive")
                 (:file "sheep")
                 (:file "peasant")
                 (:file "ui")
                 (:file "progress")
                 (:file "main"))))
  :description "Spring Lisp Game Jam 2024 entry"
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"cycle-of-evil"
  :entry-point "cycle-of-evil:main")
