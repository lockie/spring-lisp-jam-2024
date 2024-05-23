(defpackage #:prejam-2024
  (:use #:cl #:let-plus)
  (:local-nicknames (#:tiled #:cl-tiled))
  (:import-from #:alexandria
                #:array-length #:array-index #:clamp #:define-constant #:doplist
                #:format-symbol #:if-let #:make-keyword #:non-negative-fixnum
                #:positive-fixnum #:shuffle)
  (:import-from #:global-vars #:define-global-parameter)
  (:import-from #:cl-fast-behavior-trees
                #:complete-node #:define-behavior-tree
                #:define-behavior-tree/debug #:define-behavior-tree-node
                #:delete-behavior-tree #:make-behavior-tree)
  (:export #:main))
