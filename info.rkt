#lang info
(define collection "fasl-viewer")
(define deps '("base" "https://github.com/samth/kettle.git?path=kettle-lib" "compiler-lib"))
(define build-deps
  '("rackunit-lib" "https://github.com/samth/kettle.git?path=kettle-test-lib" "scribble-lib"))
(define scribblings '(("fasl-viewer.scrbl" () (tool))))
