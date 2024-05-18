(defpackage #:prejam-2024
  (:use #:cl #:let-plus)
  (:local-nicknames (#:tiled #:cl-tiled))
  (:import-from #:alexandria
                #:array-length #:array-index #:define-constant #:doplist
                #:if-let #:make-keyword #:non-negative-fixnum
                #:positive-fixnum)
  (:import-from #:global-vars #:define-global-parameter)
  (:import-from #:cl-fast-behavior-trees
                #:complete-node #:define-behavior-tree
                #:define-behavior-tree/debug #:define-behavior-tree-node
                #:make-behavior-tree)
  (:export #:main))
