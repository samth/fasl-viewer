#lang racket/base

;; analyze.rkt — Non-interactive analysis of fasl/vfasl, Racket
;; executable, and .zo files.  Prints a textual summary to stdout.

(require racket/cmdline
         racket/format
         racket/list
         racket/match
         racket/string
         compiler/zo-structs
         "parse.rkt")

(provide analyze-file
         print-analysis)

;; -------------------------------------------------------------------
;; Formatting helpers

(define (format-bytes n)
  (cond
    [(>= n (* 1024 1024)) (format "~a MB" (~r (/ n (* 1024.0 1024)) #:precision '(= 1)))]
    [(>= n 1024) (format "~a KB" (~r (/ n 1024.0) #:precision '(= 1)))]
    [else (format "~a B" n)]))

(define vspace-names
  #("symbol" "rtd" "closure" "impure" "pure-typed" "impure-record" "code" "data" "reloc"))

;; -------------------------------------------------------------------
;; Analysis output

(define (print-analysis pf)
  (define data (parsed-file-data pf))
  (printf "File: ~a\n" (parsed-file-path pf))
  (printf "Format: ~a\n\n" (parsed-file-format pf))
  (cond
    [(chez-boot-file? data)  (print-chez-analysis data)]
    [(racket-executable? data) (print-exe-analysis data)]
    [(zo-file? data) (print-zo-analysis data)]
    [(unknown-file? data) (printf "Unknown file format.\n")]))

(define (print-chez-analysis boot)
  (define headers (chez-boot-file-headers boot))
  (define objects (chez-boot-file-objects boot))

  ;; Headers
  (printf "Headers: ~a\n" (length headers))
  (for ([h (in-list headers)]
        [i (in-naturals)])
    (printf "  [~a] machine=~a  version=~a  deps=(~a)\n"
            i
            (lookup-machine-type (boot-header-machine-type h))
            (format-scheme-version (boot-header-version h))
            (boot-header-dependencies h)))

  ;; Object summary
  (printf "\nObjects: ~a\n" (length objects))

  ;; Count by kind
  (define fasl-count (count (lambda (o) (eq? (fasl-object-kind o) 'fasl)) objects))
  (define vfasl-count (count (lambda (o) (eq? (fasl-object-kind o) 'vfasl)) objects))
  (printf "  fasl: ~a   vfasl: ~a\n" fasl-count vfasl-count)

  ;; Count by situation
  (define visit-count (count (lambda (o) (eq? (fasl-object-situation o) 'visit)) objects))
  (define revisit-count (count (lambda (o) (eq? (fasl-object-situation o) 'revisit)) objects))
  (define vr-count (count (lambda (o) (eq? (fasl-object-situation o) 'visit-revisit)) objects))
  (printf "  visit: ~a   revisit: ~a   visit-revisit: ~a\n" visit-count revisit-count vr-count)

  ;; Count by compression
  (define uncomp-count (count (lambda (o) (eq? (fasl-object-compression o) 'uncompressed)) objects))
  (define lz4-count (count (lambda (o) (eq? (fasl-object-compression o) 'lz4)) objects))
  (define gzip-count (count (lambda (o) (eq? (fasl-object-compression o) 'gzip)) objects))
  (printf "  uncompressed: ~a   lz4: ~a   gzip: ~a\n" uncomp-count lz4-count gzip-count)

  ;; Total sizes
  (define total-compressed
    (for/sum ([o (in-list objects)])
      (fasl-object-compressed-size o)))
  (define total-uncompressed
    (for/sum ([o (in-list objects)])
      (fasl-object-uncompressed-size o)))
  (printf "\nTotal compressed size: ~a\n" (format-bytes total-compressed))
  (printf "Total uncompressed size: ~a\n" (format-bytes total-uncompressed))
  (when (> total-compressed 0)
    (printf "Compression ratio: ~a:1\n"
            (~r (/ total-uncompressed total-compressed 1.0) #:precision '(= 2))))

  ;; Content type breakdown for fasl objects
  (define content-types (make-hash))
  (for ([o (in-list objects)]
        #:when (eq? (fasl-object-kind o) 'fasl))
    (define inner (fasl-object-inner-info o))
    (define ct-name
      (cond
        [inner (symbol->string (fasl-inner-info-type inner))]
        [(fasl-object-content-type o)
         (lookup-fasl-content-type (fasl-object-content-type o))]
        [else "unknown"]))
    (hash-update! content-types ct-name add1 0))
  (when (positive? (hash-count content-types))
    (printf "\nFasl content types:\n")
    (for ([pair (in-list (sort (hash->list content-types)
                               > #:key cdr))])
      (printf "  ~a: ~a\n" (~a #:min-width 20 #:align 'left (car pair)) (cdr pair))))

  ;; vfasl vspace breakdown
  (define vfasl-objects (filter (lambda (o) (eq? (fasl-object-kind o) 'vfasl)) objects))
  (when (pair? vfasl-objects)
    (printf "\nvfasl vspace totals:\n")
    (define vspace-totals (make-vector (vector-length vspace-names) 0))
    (define total-data 0)
    (for ([o (in-list vfasl-objects)])
      (define vh (fasl-object-vfasl-hdr o))
      (when vh
        (define data-size (vfasl-header-data-size vh))
        (set! total-data (+ total-data data-size))
        (define rel-offsets (vfasl-header-vspace-offsets vh))
        (define all-starts (cons 0 (append rel-offsets (list data-size))))
        (for ([i (in-range (min (vector-length vspace-names) (sub1 (length all-starts))))]
              #:when (< (add1 i) (length all-starts)))
          (define sz (- (list-ref all-starts (add1 i)) (list-ref all-starts i)))
          (vector-set! vspace-totals i (+ (vector-ref vspace-totals i) sz)))))
    (for ([i (in-range (vector-length vspace-names))])
      (define sz (vector-ref vspace-totals i))
      (define pct (if (> total-data 0) (~r (* 100.0 (/ sz total-data)) #:precision '(= 1)) "0.0"))
      (printf "  ~a ~a  ~a%\n"
              (~a #:min-width 14 #:align 'left (vector-ref vspace-names i))
              (~a #:min-width 10 #:align 'right (format-bytes sz))
              (~a #:min-width 5 #:align 'right pct))))

  ;; Per-object listing
  (printf "\nObject listing:\n")
  (for ([o (in-list objects)])
    (define idx (fasl-object-index o))
    (define sit (fasl-object-situation o))
    (define comp (fasl-object-compression o))
    (define kind (fasl-object-kind o))
    (define csize (fasl-object-compressed-size o))
    (define usize (fasl-object-uncompressed-size o))
    (define inner (fasl-object-inner-info o))
    (define size-str
      (cond
        [(equal? comp 'uncompressed) (format-bytes usize)]
        [else (format "~a -> ~a" (format-bytes csize) (format-bytes usize))]))
    (define type-str
      (cond
        [inner
         (define type (fasl-inner-info-type inner))
         (define name (fasl-inner-info-name inner))
         (cond
           [(and name (not (equal? name "")))
            (format "~a ~a" type name)]
           [else (format "~a" type)])]
        [(fasl-object-content-type o)
         (lookup-fasl-content-type (fasl-object-content-type o))]
        [(fasl-object-vfasl-hdr o) "vfasl"]
        [else ""]))
    (printf "  [~a] ~a ~a ~a ~a~a\n"
            (~a #:min-width 4 #:align 'right idx)
            (~a #:min-width 14 #:align 'left sit)
            (~a #:min-width 14 #:align 'left comp)
            (~a #:min-width 6 #:align 'left kind)
            size-str
            (if (string=? type-str "") "" (format "  [~a]" type-str)))))

(define (print-exe-analysis exe)
  (printf "Section offset: ~a\n" (racket-executable-section-offset exe))
  (printf "Section size: ~a\n\n" (format-bytes (racket-executable-section-size exe)))

  (define boots (racket-executable-boot-files exe))
  (define boot-names '("petite.boot" "scheme.boot" "racket.boot"))
  (printf "Embedded boot files: ~a\n\n" (length boots))
  (for ([entry (in-list boots)]
        [bname (in-list boot-names)]
        [i (in-naturals)])
    (printf "--- ~a (offset=~a, size=~a) ---\n"
            bname
            (racket-boot-entry-offset entry)
            (format-bytes (racket-boot-entry-size entry)))
    (print-chez-analysis (racket-boot-entry-boot entry))
    (newline)))

(define (print-zo-analysis zo)
  (printf "Version: ~a\n" (zo-file-version zo))
  (printf "Machine: ~a\n" (zo-file-machine-type zo))
  (printf "Tag: ~a\n" (zo-file-tag zo))

  (define content (zo-file-content zo))
  (cond
    [(not content) (printf "\n(zo-parse failed)\n")]
    [(linkl-directory? content)
     (define tbl (linkl-directory-table content))
     (define keys (hash-keys tbl))
     (printf "\nType: linklet directory\n")
     (printf "Modules: ~a\n" (length keys))
     (for ([k (in-list (sort keys
                             (lambda (a b)
                               (cond
                                 [(and (null? a) (null? b)) #f]
                                 [(null? a) #t]
                                 [(null? b) #f]
                                 [else (string<? (format "~a" a) (format "~a" b))]))))])
       (define bundle (hash-ref tbl k))
       (define phases (sort (filter integer? (hash-keys (linkl-bundle-table bundle))) <))
       (define meta-keys (filter symbol? (hash-keys (linkl-bundle-table bundle))))
       (printf "  ~a: ~a phase(s) ~a, ~a metadata key(s)\n"
               k
               (length phases)
               (if (pair? phases) (format "~a" phases) "")
               (length meta-keys)))]
    [(linkl-bundle? content)
     (define tbl (linkl-bundle-table content))
     (define phases (sort (filter integer? (hash-keys tbl)) <))
     (define meta-keys (filter symbol? (hash-keys tbl)))
     (printf "\nType: linklet bundle\n")
     (printf "Phases: ~a ~a\n" (length phases) (if (pair? phases) (format "~a" phases) ""))
     (printf "Metadata keys: ~a ~a\n" (length meta-keys)
             (if (pair? meta-keys) (format "~a" meta-keys) ""))]
    [(linkl? content)
     (printf "\nType: single linklet\n")]
    [else
     (printf "\nType: unknown (~a)\n" content)]))

;; Convenience wrapper
(define (analyze-file path)
  (define pf (parse-file path))
  (print-analysis pf))

(module+ main
  (define file-path
    (command-line
     #:program "fasl-analyze"
     #:args (file)
     file))
  (analyze-file file-path))
