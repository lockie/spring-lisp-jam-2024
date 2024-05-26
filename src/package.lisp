(defpackage #:prejam-2024
  (:use #:cl #:let-plus)
  (:local-nicknames (#:tiled #:cl-tiled)
                    (#:ui #:cl-liballegro-nuklear/declarative))
  (:import-from #:alexandria
                #:array-length #:array-index #:clamp #:define-constant #:doplist
                #:format-symbol #:if-let #:length= #:make-keyword
                #:non-negative-fixnum #:positive-fixnum #:random-elt #:shuffle
                #:when-let #:with-gensyms)
  (:import-from #:global-vars #:define-global-parameter)
  (:import-from #:cl-fast-behavior-trees
                #:complete-node #:define-behavior-tree
                #:define-behavior-tree/debug #:define-behavior-tree-node
                #:delete-behavior-tree #:make-behavior-tree)
  (:export #:main))
