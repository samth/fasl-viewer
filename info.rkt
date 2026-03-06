#lang info
(define collection "fasl-viewer")
(define deps '("base" "kettle-lib" "compiler-lib"))
(define build-deps
  '("rackunit-lib" "kettle-test-lib" "scribble-lib"))
(define scribblings '(("fasl-viewer.scrbl" () (tool))))
