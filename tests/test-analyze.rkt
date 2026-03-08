#lang racket/base

(require rackunit
         racket/port
         racket/path
         racket/file
         racket/string
         racket/system
         racket/runtime-path
         setup/dirs
         "../analyze.rkt"
         "../parse.rkt")

;; -------------------------------------------------------------------
;; Setup: compile a .zo fixture and locate the racket executable

(define racket-exe-path (build-path (find-console-bin-dir) "racket"))
(define-runtime-path zo-fixture-src "zo-fixture.rkt")
(define zo-path
  (let ([zo (build-path (path-only zo-fixture-src) "compiled" "zo-fixture_rkt.zo")])
    (unless (file-exists? zo)
      (system* racket-exe-path "-l" "raco" "make" (path->string zo-fixture-src)))
    (and (file-exists? zo) zo)))
(define zo-available? (and zo-path #t))

(define-syntax-rule (test-case/zo name body ...)
  (when zo-available?
    (test-case name body ...)))

;; -------------------------------------------------------------------
;; format-bytes unit tests

(test-case "format-bytes: bytes"
  (check-equal? (format-bytes 0) "0 B")
  (check-equal? (format-bytes 512) "512 B")
  (check-equal? (format-bytes 1023) "1023 B"))

(test-case "format-bytes: kilobytes"
  (check-regexp-match #rx"KB" (format-bytes 1024))
  (check-regexp-match #rx"KB" (format-bytes 8192))
  (check-regexp-match #rx"1\\.0 KB" (format-bytes 1024)))

(test-case "format-bytes: megabytes"
  (check-regexp-match #rx"MB" (format-bytes (* 1024 1024)))
  (check-regexp-match #rx"1\\.0 MB" (format-bytes (* 1024 1024)))
  (check-regexp-match #rx"MB" (format-bytes (* 5 1024 1024))))

;; -------------------------------------------------------------------
;; print-analysis on racket executable

(test-case "print-analysis: racket executable produces expected output"
  (define pf (parse-file racket-exe-path))
  (define output (with-output-to-string (lambda () (print-analysis pf))))
  ;; Should contain file path and format
  (check-regexp-match #rx"File:" output)
  (check-regexp-match #rx"Format:" output)
  ;; Should show embedded boot files section
  (check-regexp-match #rx"Embedded boot files:" output)
  ;; Should mention petite.boot, scheme.boot, racket.boot
  (check-regexp-match #rx"petite\\.boot" output)
  (check-regexp-match #rx"scheme\\.boot" output)
  (check-regexp-match #rx"racket\\.boot" output)
  ;; Should have Headers and Objects sections for each boot file
  (check-regexp-match #rx"Headers:" output)
  (check-regexp-match #rx"Objects:" output)
  ;; Should show size information
  (check-regexp-match #rx"Total compressed size:" output)
  (check-regexp-match #rx"Total uncompressed size:" output))

;; -------------------------------------------------------------------
;; print-analysis on .zo file

(test-case/zo "print-analysis: zo file produces expected output"
  (define pf (parse-file zo-path))
  (define output (with-output-to-string (lambda () (print-analysis pf))))
  ;; Should contain file path and format
  (check-regexp-match #rx"File:" output)
  (check-regexp-match #rx"Format:" output)
  ;; Should show zo-specific information
  (check-regexp-match #rx"Version:" output)
  (check-regexp-match #rx"Machine:" output)
  (check-regexp-match #rx"Tag:" output)
  ;; Should identify the linklet type
  (check-true (or (regexp-match? #rx"linklet directory" output)
                  (regexp-match? #rx"linklet bundle" output)
                  (regexp-match? #rx"single linklet" output))
              "should identify the linklet type"))

(test-case/zo "print-analysis: zo linklet directory lists modules"
  (define pf (parse-file zo-path))
  (define output (with-output-to-string (lambda () (print-analysis pf))))
  ;; zo-fixture.rkt is a simple module, should produce a linklet directory or bundle
  ;; with at least one phase
  (check-regexp-match #rx"phase" output))

;; -------------------------------------------------------------------
;; analyze-file convenience wrapper

(test-case/zo "analyze-file runs without error on zo"
  (check-not-exn
   (lambda ()
     (with-output-to-string (lambda () (analyze-file (path->string zo-path)))))))

(test-case "analyze-file runs without error on racket exe"
  (check-not-exn
   (lambda ()
     (with-output-to-string (lambda () (analyze-file (path->string racket-exe-path)))))))

;; -------------------------------------------------------------------
;; raco command registration (via info.rkt in project root)

(define-runtime-path info-path "../info.rkt")

(test-case "raco fasl-analyze command is registered in info.rkt"
  (define info-content (file->string (path->string info-path)))
  ;; info.rkt should mention both raco commands
  (check-regexp-match #rx"fasl-viewer" info-content)
  (check-regexp-match #rx"fasl-analyze" info-content)
  (check-regexp-match #rx"raco-commands" info-content)
  ;; Should reference the analyze module
  (check-regexp-match #rx"fasl-viewer/analyze" info-content))

;; -------------------------------------------------------------------
;; analyze.rkt main submodule invocation

(define-runtime-path analyze-module "../analyze.rkt")

(test-case/zo "analyze.rkt main submodule runs on zo file"
  (define ok?
    (system* racket-exe-path (path->string analyze-module) (path->string zo-path)))
  (check-true ok? "analyze.rkt main should exit successfully on zo"))

(test-case "analyze.rkt main submodule runs on racket exe"
  (define ok?
    (system* racket-exe-path (path->string analyze-module) (path->string racket-exe-path)))
  (check-true ok? "analyze.rkt main should exit successfully on racket exe"))
