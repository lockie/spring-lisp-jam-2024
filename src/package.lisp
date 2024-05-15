(defpackage #:prejam-2024
  (:use #:cl #:let-plus)
  (:local-nicknames (#:tiled #:cl-tiled))
  (:import-from #:alexandria
                #:define-constant #:doplist #:make-keyword
                #:non-negative-fixnum)
  (:export #:main))
