#lang racket/base

;; analyze.rkt — Non-interactive analysis of fasl/vfasl, Racket
;; executable, and .zo files.  Prints a textual summary to stdout.

(require racket/cmdline
         racket/format
         racket/list
         racket/match
         racket/string
         compiler/zo-structs
         compiler/decompile
         "parse.rkt")

(provide analyze-file
         print-analysis
         format-bytes)

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
;; Common "named item with size" display
;;
;; An item is (list name size detail) where:
;;   name   : string — what it is
;;   size   : exact-nonneg-integer — bytes
;;   detail : string or #f — extra info (source loc, phase, etc.)

;; Print the top N items by size.
(define (print-largest-items items
                            #:label [label "largest items"]
                            #:limit [limit 15]
                            #:total-label [total-label #f])
  (define total (for/sum ([i (in-list items)]) (cadr i)))
  (when (and total-label (> total 0))
    (printf "  ~a: ~a\n" total-label (format-bytes total)))
  (define sorted (sort items > #:key cadr))
  (define top-n (take sorted (min limit (length sorted))))
  (when (pair? top-n)
    (printf "  ~a:\n" label)
    (define top-n-total (for/sum ([d (in-list top-n)]) (cadr d)))
    (for ([d (in-list top-n)])
      (match-define (list name size detail) d)
      (define pct (if (> total 0) (* 100.0 (/ size total)) 0))
      (printf "    ~a  ~a  ~a~a\n"
              (~a #:min-width 40 #:align 'left name)
              (~a #:min-width 10 #:align 'right (format-bytes size))
              (~a #:min-width 6 #:align 'right
                  (format "~a%" (~r pct #:precision '(= 1))))
              (if detail (format "  ~a" detail) "")))
    (when (> total 0)
      (define top-pct (~r (* 100.0 (/ top-n-total total)) #:precision '(= 1)))
      (printf "  (top ~a: ~a of ~a, ~a%)\n"
              (length top-n) (format-bytes top-n-total) (format-bytes total) top-pct))))

;; -------------------------------------------------------------------
;; Fasl object → items

;; Derive the display name for a fasl object.
(define (fasl-object-name o)
  (define inner (fasl-object-inner-info o))
  (cond
    [inner
     (define type (fasl-inner-info-type inner))
     (define name (fasl-inner-info-name inner))
     (if (and name (not (equal? name "")))
         (format "~a ~a" type name)
         (format "~a" type))]
    [(fasl-object-content-type o)
     (lookup-fasl-content-type (fasl-object-content-type o))]
    [(fasl-object-vfasl-hdr o) "vfasl"]
    [else (format "~a #~a" (fasl-object-kind o) (fasl-object-index o))]))

;; Convert fasl objects to items for the common display.
(define (fasl-objects->items objects)
  (for/list ([o (in-list objects)])
    (list (fasl-object-name o)
          (fasl-object-uncompressed-size o)
          #f)))

;; -------------------------------------------------------------------
;; Zo decompilation → items

;; Search an S-expression tree for a #%machine-code form and return
;; the byte-string length, or #f.
(define (find-machine-code-size tree)
  (cond
    [(not (pair? tree)) #f]
    [(and (eq? (car tree) '#%machine-code)
          (pair? (cdr tree))
          (bytes? (cadr tree)))
     (bytes-length (cadr tree))]
    [else (or (find-machine-code-size (car tree))
              (find-machine-code-size (cdr tree)))]))

;; Search for a source-location string like "/path/file.rkt:10:0"
(define (find-source-loc tree)
  (cond
    [(string? tree) (and (regexp-match? #rx"\\.[a-z]+:" tree) tree)]
    [(not (pair? tree)) #f]
    [else (or (find-source-loc (car tree))
              (find-source-loc (cdr tree)))]))

;; Shorten a source location to just the filename + position.
(define (short-source-loc loc)
  (cond
    [(not loc) #f]
    [(regexp-match #rx"([^/]+\\.[a-z]+:[0-9]+:[0-9]+)$" loc) => cadr]
    [else loc]))

;; Extract items from a single decompiled form.
(define (extract-def-items form #:phase [phase 0])
  (define (make-item name body)
    (define code-size (find-machine-code-size body))
    (if code-size
        (list (list (format "~a" name) code-size
                    (let ([loc (short-source-loc (find-source-loc body))])
                      (cond
                        [(and loc (not (= phase 0)))
                         (format "~a  [phase ~a]" loc phase)]
                        [loc loc]
                        [(not (= phase 0))
                         (format "[phase ~a]" phase)]
                        [else #f]))))
        '()))
  (cond
    [(and (pair? form) (eq? (car form) 'define))
     (make-item (cadr form) (cddr form))]
    [(and (pair? form) (eq? (car form) 'define-values))
     (make-item (cadr form) (cddr form))]
    [else '()]))

;; Collect items from a decompiled linklet.
(define (linklet-items linklet-val #:phase [phase 0])
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define result (decompile linklet-val))
    (if (list? result)
        (apply append (for/list ([form (in-list result)])
                        (extract-def-items form #:phase phase)))
        '())))

;; Collect items from all phases in a bundle.
(define (bundle-items bundle)
  (define tbl (linkl-bundle-table bundle))
  (define phases (sort (filter integer? (hash-keys tbl)) <))
  (apply append
         (for/list ([phase (in-list phases)])
           (linklet-items (hash-ref tbl phase) #:phase phase))))

;; Extract provide/require forms from a list of decompiled forms.
(define (extract-provides+requires forms)
  (define provides '())
  (define requires '())
  (when (list? forms)
    (for ([form (in-list forms)])
      (when (pair? form)
        (cond
          [(eq? (car form) 'provide)
           (set! provides (append provides (cdr form)))]
          [(eq? (car form) 'require)
           (set! requires (append requires (cdr form)))]))))
  (values provides requires))

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

  ;; Largest objects
  (print-largest-items (fasl-objects->items objects)
                       #:label "largest objects"))

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

;; -------------------------------------------------------------------
;; Zo analysis

(define (print-bundle-analysis bundle)
  (define tbl (linkl-bundle-table bundle))
  (define phases (sort (filter integer? (hash-keys tbl)) <))
  (define meta-keys (filter symbol? (hash-keys tbl)))

  (printf "  phases: ~a ~a, ~a metadata key(s)\n"
          (length phases)
          (if (pair? phases) (format "~a" phases) "")
          (length meta-keys))

  (print-largest-items (bundle-items bundle)
                       #:label "largest definitions"
                       #:total-label "total code"))

(define (print-zo-analysis zo)
  (printf "Version: ~a\n" (zo-file-version zo))
  (printf "Machine: ~a\n" (zo-file-machine-type zo))
  (printf "Tag: ~a\n" (zo-file-tag zo))

  (define content (zo-file-content zo))

  ;; Decompile the whole zo for top-level provide/require info.
  (define top-forms
    (and content
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (define r (decompile content))
           (and (list? r) r))))

  (define (print-provides+requires)
    (when top-forms
      (define-values (provides requires) (extract-provides+requires top-forms))
      (when (pair? requires)
        (printf "Requires: ~a\n" (string-join (map (lambda (e) (format "~a" e)) requires) " ")))
      (when (pair? provides)
        (printf "Provides: ~a\n" (string-join (map (lambda (e) (format "~a" e)) provides) " ")))))

  (cond
    [(not content) (printf "\n(zo-parse failed)\n")]
    [(linkl-directory? content)
     (define tbl (linkl-directory-table content))
     (define keys (hash-keys tbl))
     (printf "\nType: linklet directory\n")
     (printf "Modules: ~a\n" (length keys))

     (print-provides+requires)

     (define sorted-keys
       (sort keys (lambda (a b)
                    (cond
                      [(and (null? a) (null? b)) #f]
                      [(null? a) #t]
                      [(null? b) #f]
                      [else (string<? (format "~a" a) (format "~a" b))]))))
     (for ([k (in-list sorted-keys)])
       (define bundle (hash-ref tbl k))
       (printf "\n--- module ~a ---\n" (if (null? k) "(root)" k))
       (print-bundle-analysis bundle))]
    [(linkl-bundle? content)
     (printf "\nType: linklet bundle\n")
     (print-provides+requires)
     (print-bundle-analysis content)]
    [(linkl? content)
     (printf "\nType: single linklet\n")
     (print-largest-items (linklet-items content)
                          #:label "largest definitions"
                          #:total-label "total code")]
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
