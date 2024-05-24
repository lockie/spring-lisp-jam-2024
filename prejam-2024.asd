(defsystem "prejam-2024"
  :version "0.0.1"
  :author "Andrew Kravchuk"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-aseprite
               #:cl-fast-behavior-trees
               #:cl-fast-ecs
               #:cl-liballegro
               #:cl-liballegro-nuklear
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
                 (:file "map")
                 (:file "poison")
                 (:file "fire")
                 (:file "projectile")
                 (:file "priority-queue")
                 (:file "character")
                 (:file "offensive")
                 (:file "sheep")
                 (:file "main"))))
  :description "A simple game."
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"prejam-2024"
  :entry-point "prejam-2024:main")
