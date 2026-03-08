#lang info
(define collection "fasl-viewer")
(define deps '("base" "kettle-lib" "compiler-lib"))
(define build-deps
  '("rackunit-lib" "kettle-test-lib" "scribble-lib"))
(define scribblings '(("fasl-viewer.scrbl" () (tool))))
(define raco-commands
  '(("fasl-viewer" (submod fasl-viewer/main main) "interactive TUI viewer for fasl, boot, and zo files" #f)
    ("fasl-analyze" (submod fasl-viewer/analyze main) "print analysis of fasl, boot, and zo files" #f)))
