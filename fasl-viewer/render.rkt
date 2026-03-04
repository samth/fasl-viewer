#lang racket/base

;; render.rkt — Build a tree of nodes from parsed data, and render
;; visible lines as kettle images with colors and box-drawing.

(require racket/string
         racket/format
         racket/list
         racket/match
         racket/set
         racket/pretty
         kettle
         compiler/zo-structs
         compiler/decompile
         "parse.rkt")

(provide build-tree
         flatten-tree
         render-view
         annotate-asm-registers

         ;; Tree node structure
         (struct-out tnode)
         (struct-out flat-item))

;; A tree node: id, label, optional detail lines, children, expandable flag
(struct tnode (id label details children expandable?) #:transparent)

;; Vspace names corresponding to the enumeration in cmacros.ss
(define vspace-names
  #("symbol" "rtd" "closure" "impure" "pure-typed" "impure-record" "code" "data" "reloc"))

(define vspace-colors
  (vector fg-magenta
          fg-red
          fg-green
          fg-yellow
          fg-blue
          fg-bright-red
          fg-cyan
          fg-bright-yellow
          fg-bright-magenta))

(define (format-bytes n)
  (cond
    [(>= n (* 1024 1024)) (format "~a MB" (~r (/ n (* 1024.0 1024)) #:precision '(= 1)))]
    [(>= n 1024) (format "~a KB" (~r (/ n 1024.0) #:precision '(= 1)))]
    [else (format "~a B" n)]))

;; -------------------------------------------------------------------
;; Build tree from parsed file

(define (build-tree pf)
  (define data (parsed-file-data pf))
  (define basename
    (let ([p (parsed-file-path pf)])
      (define parts (regexp-split #rx"/" p))
      (last parts)))
  (cond
    [(chez-boot-file? data) (build-chez-tree basename data)]
    [(racket-executable? data) (build-exe-tree basename data)]
    [(zo-file? data) (build-zo-tree basename data)]
    [(unknown-file? data) (list (tnode 'root (format "~a  (unknown format)" basename) '() '() #f))]))

(define (build-chez-tree name boot [prefix ""])
  (define headers (chez-boot-file-headers boot))
  (define objects (chez-boot-file-objects boot))
  (define first-hdr (and (pair? headers) (first headers)))
  (define version-str
    (if first-hdr
        (format-scheme-version (boot-header-version first-hdr))
        "?"))
  (define machine-str
    (if first-hdr
        (lookup-machine-type (boot-header-machine-type first-hdr))
        "?"))

  (define (make-id s) (string->symbol (string-append prefix s)))

  ;; Build header children (deduplicated — repeated headers are concatenation artifacts)
  (define seen-headers (make-hash))
  (define header-children
    (filter
     values
     (for/list ([h (in-list headers)]
                [i (in-naturals)])
       (define deps (boot-header-dependencies h))
       (define mt-num (boot-header-machine-type h))
       (define ver (boot-header-version h))
       (define key (list mt-num ver deps))
       (cond
         [(hash-ref seen-headers key #f) #f]
         [else
          (hash-set! seen-headers key #t)
          (define mt (lookup-machine-type mt-num))
          (tnode (make-id (format "h~a" i))
                 (format "~a  v~a~a"
                         mt
                         (format-scheme-version ver)
                         (if (string=? deps "")
                             ""
                             (format "  deps=(~a)" deps)))
                 '()
                 '()
                 #f)]))))

  ;; Build object children
  (define object-children
    (for/list ([obj (in-list objects)])
      (build-object-node obj prefix)))

  (list
   (tnode
    (make-id "headers")
    (format "~a  (~a, v~a, ~a headers, ~a objects)"
            name
            machine-str
            version-str
            (length header-children)
            (length objects))
    '()
    (append
     (if (pair? header-children)
         (list (tnode (make-id "hdr-group") (format "Headers (~a)" (length header-children)) '() header-children #t))
         '())
     object-children)
    #t)))

(define (inner-info->bracket-text inner)
  (define type (fasl-inner-info-type inner))
  (define name (fasl-inner-info-name inner))
  (define name? (and name (not (equal? name ""))))
  (cond
    [(and (eq? type 'immediate) name) name]
    [(and (eq? type 'begin) name) (format "begin(~a)" name)]
    [(and name? (memq type '(code closure)))
     (format "~a ~s" type name)]
    [name? (format "~a ~a" type name)]
    [else (format "~a" type)]))

(define (build-fasl-inner-details inner)
  (define type (fasl-inner-info-type inner))
  (define name (fasl-inner-info-name inner))
  (define gc (fasl-inner-info-graph-count inner))
  (define ge (fasl-inner-info-graph-external inner))
  (append
   (list (format "type: ~a" type))
   (if (and name (not (equal? name "")))
       (list (format "name: ~a" name))
       '())
   (if (and gc ge)
       (list (format "graph: ~a entries, ~a external" gc ge))
       '())))

(define (build-object-node obj [prefix ""])
  (define idx (fasl-object-index obj))
  (define sit (fasl-object-situation obj))
  (define comp (fasl-object-compression obj))
  (define kind (fasl-object-kind obj))
  (define csize (fasl-object-compressed-size obj))
  (define usize (fasl-object-uncompressed-size obj))
  (define ct (fasl-object-content-type obj))
  (define vh (fasl-object-vfasl-hdr obj))
  (define inner (fasl-object-inner-info obj))

  (define size-str
    (cond
      [(equal? comp 'uncompressed) (format-bytes usize)]
      [else (format "~a -> ~a" (format-bytes csize) (format-bytes usize))]))

  (define type-str
    (cond
      [inner (inner-info->bracket-text inner)]
      [ct (lookup-fasl-content-type ct)]
      [vh "vfasl"]
      [else ""]))

  (define label
    (format "~a  ~a  ~a  ~a~a"
            sit
            comp
            kind
            size-str
            (if (string=? type-str "")
                ""
                (format "  [~a]" type-str))))

  ;; Build detail lines for expanded view
  (define details
    (cond
      [vh (build-vfasl-details vh)]
      [inner (build-fasl-inner-details inner)]
      [else '()]))

  (tnode (string->symbol (string-append prefix (format "o~a" idx))) label details '() (pair? details)))

(define (build-vfasl-details vh)
  (define data-size (vfasl-header-data-size vh))
  (define rel-offsets (vfasl-header-vspace-offsets vh))
  (define all-starts (cons 0 (append rel-offsets (list data-size))))

  (append (list (format "data: ~a  table: ~a  result-offset: ~a"
                        (format-bytes data-size)
                        (format-bytes (vfasl-header-table-size vh))
                        (vfasl-header-result-offset vh))
                (format "symrefs: ~a  rtdrefs: ~a  singletonrefs: ~a"
                        (vfasl-header-symref-count vh)
                        (vfasl-header-rtdref-count vh)
                        (vfasl-header-singletonref-count vh)))
          (for/list ([i (in-range (min (vector-length vspace-names) (sub1 (length all-starts))))]
                     #:when (< (add1 i) (length all-starts)))
            (define sz (- (list-ref all-starts (add1 i)) (list-ref all-starts i)))
            (define pct
              (if (> data-size 0)
                  (~r (* 100.0 (/ sz data-size)) #:precision '(= 1))
                  "0.0"))
            (format "~a ~a  ~a%"
                    (~a #:min-width 14 #:align 'left (vector-ref vspace-names i))
                    (~a #:min-width 10 #:align 'right (format-bytes sz))
                    (~a #:min-width 5 #:align 'right pct)))))

;; -------------------------------------------------------------------
;; Racket executable tree

(define (build-exe-tree name exe)
  (define boots (racket-executable-boot-files exe))
  (define boot-names '("petite.boot" "scheme.boot" "racket.boot"))
  (define boot-children
    (for/list ([entry (in-list boots)]
               [bname (in-list boot-names)]
               [i (in-naturals)])
      (define inner (build-chez-tree bname (racket-boot-entry-boot entry) (format "boot~a/" i)))
      ;; Merge the inner chez root into the exe boot entry: use the chez root's
      ;; label (which has machine/version/counts) and its children directly,
      ;; adding offset/size info.
      (define chez-root (first inner))
      (tnode (string->symbol (format "boot~a" i))
             (format "~a  offset ~a, ~a"
                     (tnode-label chez-root)
                     (racket-boot-entry-offset entry)
                     (format-bytes (racket-boot-entry-size entry)))
             (tnode-details chez-root)
             (tnode-children chez-root)
             (tnode-expandable? chez-root))))

  (list (tnode 'root
               (format "~a  (.rackboot ~a at offset ~a)"
                       name
                       (format-bytes (racket-executable-section-size exe))
                       (racket-executable-section-offset exe))
               '()
               boot-children
               #t)))

;; -------------------------------------------------------------------
;; .zo tree

(define (pretty-print-to-string v)
  (define out (open-output-string))
  (pretty-print v out)
  (string-trim (get-output-string out) #:left? #f))

(define (sexp->detail-lines v)
  (string-split (pretty-print-to-string v) "\n"))

;; Extract assembly string lines from an #%assembly-code form,
;; stripping the sexp wrapper and trailing empty strings.
(define (assembly-form->lines form)
  (and (pair? form)
       (eq? (car form) '#%assembly-code)
       (let ([lines (filter (lambda (s) (and (string? s) (not (equal? s ""))))
                            (cdr form))])
         (and (pair? lines) lines))))

;; Walk a sexp tree and extract #%assembly-code forms inline.
;; Non-assembly parts are pretty-printed normally; assembly strings
;; are emitted directly so they get assembly-line coloring.
(define (sexp->detail-lines/asm v)
  (define (has-assembly? v)
    (and (pair? v)
         (or (eq? (car v) '#%assembly-code)
             (for/or ([sub (in-list v)])
               (and (pair? sub) (has-assembly? sub))))))
  (cond
    [(not (has-assembly? v)) (sexp->detail-lines v)]
    [(and (pair? v) (eq? (car v) '#%assembly-code))
     (or (assembly-form->lines v) (sexp->detail-lines v))]
    [else
     ;; Walk the list, printing non-assembly sub-forms normally
     ;; and extracting assembly strings inline
     (apply append
            (for/list ([sub (in-list v)])
              (cond
                [(and (pair? sub) (eq? (car sub) '#%assembly-code))
                 (or (assembly-form->lines sub) (sexp->detail-lines sub))]
                [(and (pair? sub) (has-assembly? sub))
                 (sexp->detail-lines/asm sub)]
                [else (sexp->detail-lines sub)])))]))

(define (decompile-to-details v)
  (with-handlers ([exn:fail? (lambda (e)
                                (list (format "(decompile error: ~a)" (exn-message e))))])
    (define result (decompile v))
    (cond
      [(list? result)
       (apply append (for/list ([form (in-list result)])
                       (sexp->detail-lines/asm form)))]
      [else (sexp->detail-lines/asm result)])))

(define (build-linkl-bundle-children bundle prefix)
  (define tbl (linkl-bundle-table bundle))
  (define keys (sort (hash-keys tbl)
                     (lambda (a b)
                       (cond
                         [(and (integer? a) (integer? b)) (< a b)]
                         [(integer? a) #t]
                         [(integer? b) #f]
                         [else (symbol<? a b)]))))
  (for/list ([k (in-list keys)])
    (define v (hash-ref tbl k))
    (define id (string->symbol (string-append prefix (format "k~a" k))))
    (cond
      [(integer? k)
       (define details (decompile-to-details v))
       (tnode id (format "Phase ~a linklet" k) details '() (pair? details))]
      [else
       (define details
         (with-handlers ([exn:fail? (lambda (_) (list (format "~a" v)))])
           (define result (decompile v))
           (cond
             ;; decompile wraps simple values in (quote val) — unwrap for display
             [(and (pair? result) (eq? (car result) 'quote) (= (length result) 2))
              (sexp->detail-lines (cadr result))]
             ;; linklet structs return a list of forms
             [(list? result)
              (apply append (for/list ([form (in-list result)])
                              (sexp->detail-lines/asm form)))]
             [else (sexp->detail-lines/asm result)])))
       ;; If the only detail line is "#<opaque>", make non-expandable
       (define opaque? (and (= (length details) 1) (equal? (first details) "#<opaque>")))
       (tnode id (format "'~a~a" k (if opaque? "  (opaque)" ""))
              (if opaque? '() details) '() (and (not opaque?) (pair? details)))])))

(define (build-zo-tree name zo)
  (define content (zo-file-content zo))
  (define header-details
    (list (format "version: ~a" (zo-file-version zo))
          (format "machine: ~a" (zo-file-machine-type zo))
          (format "tag: ~a" (zo-file-tag zo))))
  (cond
    [(not content)
     (list (tnode 'root
                  (format "~a  (Racket .zo)" name)
                  (append header-details (list "" "(zo-parse failed)"))
                  '()
                  #t))]
    [(linkl-directory? content)
     (define tbl (linkl-directory-table content))
     (define keys (sort (hash-keys tbl)
                        (lambda (a b)
                          (cond
                            [(and (null? a) (null? b)) #f]
                            [(null? a) #t]
                            [(null? b) #f]
                            [else (string<? (format "~a" a) (format "~a" b))]))))
     (define children
       (for/list ([k (in-list keys)]
                  [i (in-naturals)])
         (define bundle (hash-ref tbl k))
         (define prefix (format "mod~a/" i))
         (define mod-label (format "Module ~a" k))
         (define bundle-children (build-linkl-bundle-children bundle prefix))
         (tnode (string->symbol (format "mod~a" i))
                mod-label
                '()
                bundle-children
                (pair? bundle-children))))
     (list (tnode 'root
                  (format "~a  (Racket .zo)" name)
                  header-details
                  children
                  #t))]
    [(linkl-bundle? content)
     (define children (build-linkl-bundle-children content "b/"))
     (list (tnode 'root
                  (format "~a  (Racket .zo)" name)
                  header-details
                  children
                  #t))]
    [(linkl? content)
     (define details (append header-details (list "") (decompile-to-details content)))
     (list (tnode 'root
                  (format "~a  (Racket .zo)" name)
                  details
                  '()
                  #t))]
    [else
     (list (tnode 'root
                  (format "~a  (Racket .zo)" name)
                  (append header-details (list "" (format "(unknown zo content type: ~a)" content)))
                  '()
                  #t))]))

;; -------------------------------------------------------------------
;; Flatten tree into visible lines

;; A flat-item is one visible line in the display
(struct flat-item (id depth label expandable? expanded? is-detail?) #:transparent)

(define (flatten-tree nodes expanded [depth 0])
  (apply append
         (for/list ([n (in-list nodes)])
           (define id (tnode-id n))
           (define exp? (set-member? expanded id))
           (cons (flat-item id depth (tnode-label n) (tnode-expandable? n) exp? #f)
                 (if exp?
                     ;; Detail lines
                     (append (for/list ([d (in-list (tnode-details n))])
                               (flat-item #f (add1 depth) d #f #f #t))
                             ;; Children
                             (flatten-tree (tnode-children n) expanded (add1 depth)))
                     '())))))

;; -------------------------------------------------------------------
;; Render the visible portion as a kettle image

(define header-style (make-style #:bold #t #:reverse #t))
(define footer-style (make-style #:bold #t #:reverse #t))
(define cursor-style (make-style #:reverse #t))
(define detail-style (make-style #:foreground fg-bright-white #:italic #t))
(define asm-hex-style (make-style #:foreground fg-white))
(define asm-sexp-style (make-style #:foreground fg-cyan))
(define asm-annot-style (make-style #:foreground fg-bright-black))

;; Chez Scheme register conventions.
;; x86_64 and arm64 register names don't overlap, so one table works for both.
(define chez-register-names
  (hash ;; x86_64 (ta6le, a6le, etc.)
        "rbp"  "ac0"     ; accumulator 0
        "r10"  "ac1"     ; accumulator 1
        "r12"  "xp"      ; extra pointer
        "r11"  "yp"      ; y pointer
        "r15"  "cp"      ; continuation pointer
        "rax"  "ts"      ; temp scheme / C return
        "rbx"  "td"      ; temp data
        "r14"  "tc"      ; thread context
        "r13"  "sfp"     ; scheme frame pointer
        "r9"   "ap"      ; allocation pointer
        "r14b" "tc"  "r13b" "sfp"  "r9b" "ap"
        "r8b"  "Carg5"  "r8" "Carg5"
        "rdi"  "Carg1"  "rsi" "Carg2"  "rdx" "Carg3"
        "rcx"  "Carg4"  "rsp" "sp"
        ;; arm64 (tarm64le, arm64le, etc.)
        "x19"  "tc"      ; thread context
        "x20"  "sfp"     ; scheme frame pointer
        "x21"  "ap"      ; allocation pointer
        "x22"  "trap"    ; trap register
        "x23"  "ac0"     ; accumulator 0
        "x24"  "xp"      ; extra pointer
        "x25"  "td"      ; temp data
        "x26"  "cp"      ; continuation pointer
        "x8"   "ts"      ; temp scheme / C return
        "x0"   "Carg1"  "x1" "Carg2"  "x2" "Carg3"
        "x3"   "Carg4"  "x4" "Carg5"  "x5" "Carg6"
        "x6"   "Carg7"  "x7" "Carg8"))

;; Annotate an assembly sexp string with Chez register roles.
;; Returns the annotation string (empty if no interesting registers).
(define (annotate-asm-registers sexp-str)
  (define regs-found
    (for/list ([(hw-name chez-name) (in-hash chez-register-names)]
               #:when (regexp-match? (pregexp (string-append "\\b" hw-name "\\b")) sexp-str))
      (cons hw-name chez-name)))
  ;; Deduplicate by chez-name, keep only non-C-arg registers for brevity
  (define dominated (list->set (list "Carg1" "Carg2" "Carg3" "Carg4" "Carg5"
                                     "Carg6" "Carg7" "Carg8" "sp")))
  (define interesting
    (remove-duplicates
     (for/list ([p (in-list regs-found)]
                #:unless (set-member? dominated (cdr p)))
       (cdr p))))
  (if (null? interesting)
      ""
      (string-append " ; " (string-join (sort interesting string<?) ", "))))
(define index-style (make-style #:foreground fg-bright-black #:bold #t))

(define kind-colors (hasheq 'fasl fg-blue 'vfasl fg-magenta))

(define sit-colors (hasheq 'visit fg-green 'revisit fg-yellow 'visit-revisit fg-cyan))

(define comp-colors (hasheq 'uncompressed fg-white 'lz4 fg-green 'gzip fg-yellow))

(define search-style (make-style #:bold #t))

(define (render-view items cursor scroll width height file-path total-items
                     #:search-mode? [search-mode? #f]
                     #:search-query [search-query ""])
  (define content-height (- height 2)) ; header + footer
  (define visible
    (take (drop items (min scroll (length items)))
          (min content-height (max 0 (- (length items) scroll)))))

  ;; Header bar
  (define basename (let ([parts (regexp-split #rx"/" file-path)]) (last parts)))
  (define hdr-text (format " ~a " basename))
  (define hdr-pad (make-string (max 0 (- width (string-length hdr-text))) #\space))
  (define header-img (styled header-style (text (string-append hdr-text hdr-pad))))

  ;; Content lines
  (define line-imgs
    (for/list ([item (in-list visible)]
               [vi (in-naturals)])
      (define actual-idx (+ scroll vi))
      (define at-cursor? (= actual-idx cursor))
      (render-line item width at-cursor?)))

  ;; Pad remaining lines
  (define pad-count (max 0 (- content-height (length line-imgs))))
  (define pad-imgs (make-list pad-count (text "")))

  ;; Footer bar
  (define footer-img
    (cond
      [search-mode?
       (define search-text (format "/~a" search-query))
       (define pad (make-string (max 0 (- width (string-length search-text))) #\space))
       (styled search-style (text (string-append search-text pad)))]
      [else
       (define pct
         (if (<= total-items 0)
             100
             (min 100 (inexact->exact (round (* 100 (/ (+ cursor 1) total-items)))))))
       (define footer-text
         (format " ~a/~a  ~a%%  [j/k]move [/]search [q]quit " (add1 cursor) total-items pct))
       (define footer-pad (make-string (max 0 (- width (string-length footer-text))) #\space))
       (styled footer-style (text (string-append footer-text footer-pad)))]))

  (apply vcat 'left header-img (append line-imgs pad-imgs (list footer-img))))

(define (render-line item width at-cursor?)
  (match item
    [(flat-item id depth label expandable? expanded? is-detail?)
     (define indent (make-string (* depth 2) #\space))
     (define arrow
       (cond
         [(not expandable?) "  "]
         [expanded? "▾ "]
         [else "▸ "]))
     (define raw (string-append indent arrow label))
     (define truncated
       (if (> (string-length raw) width)
           (string-append (substring raw 0 (max 0 (- width 1))) "~")
           raw))
     (define padded
       (if (< (string-length truncated) width)
           (string-append truncated (make-string (- width (string-length truncated)) #\space))
           truncated))

     (cond
       [at-cursor? (styled cursor-style (text padded))]
       [is-detail?
        ;; Color vspace lines
        (define vspace-match
          (for/or ([i (in-range (vector-length vspace-names))])
            (and (regexp-match? (regexp (string-append "^ *" (vector-ref vspace-names i) " "))
                                label)
                 i)))
        (cond
          [vspace-match
           (styled (make-style #:foreground (vector-ref vspace-colors vspace-match)) (text padded))]
          ;; Assembly lines: hex address pattern — split hex from sexp mnemonic
          [(regexp-match #rx"^( +[0-9a-f]+: [0-9a-f]+ +)(\\(.*)" label)
           => (lambda (m)
                (define hex-part (string-append indent arrow (second m)))
                (define sexp-part (third m))
                ;; Strip any existing "; ..." comment from sexp-part before annotating
                (define-values (sexp-clean existing-comment)
                  (let ([cm (regexp-match #rx"^(.*?)(\\s+;\\s+.*)$" sexp-part)])
                    (if cm (values (second cm) (third cm)) (values sexp-part ""))))
                (define annot (annotate-asm-registers sexp-clean))
                (define comment (string-append existing-comment annot))
                (define hex-len (string-length hex-part))
                (define sexp-clean-len (string-length sexp-clean))
                (define comment-len (string-length comment))
                (define total (+ hex-len sexp-clean-len comment-len))
                (define pad (max 0 (- width total)))
                (if (equal? comment "")
                    (hcat 'left
                          (styled asm-hex-style (text hex-part))
                          (styled asm-sexp-style (text (string-append sexp-clean (make-string pad #\space)))))
                    (hcat 'left
                          (styled asm-hex-style (text hex-part))
                          (styled asm-sexp-style (text sexp-clean))
                          (styled asm-annot-style (text (string-append comment (make-string pad #\space)))))))]
          ;; RELOC lines
          [(regexp-match? #rx"^RELOC " label)
           (styled asm-hex-style (text padded))]
          [else
           (styled detail-style (text padded))])]
       ;; Color based on content for object lines
       [(and (not is-detail?) (not expandable?) id)
        ;; Leaf item (header entry etc)
        (styled (make-style #:foreground fg-white) (text padded))]
       [else (text padded)])]))
