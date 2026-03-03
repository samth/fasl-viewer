#lang racket/base

;; describe.rkt -- Deep inspection of fasl entries using Chez's
;; $describe-fasl-from-port primitive, with optional disassembly.
;;
;; Each entry is described independently (single-entry boot file stream)
;; to handle boot files with mixed fasl/vfasl entries. The version in
;; the header is patched to match the running Chez VM to avoid version
;; mismatch errors.

(require ffi/unsafe/vm
         compiler/private/chez
         racket/pretty
         racket/port
         racket/string
         racket/list
         racket/match
         racket/set
         racket/file
         "render.rkt")

(provide describe-boot-file-entries
         describe-exe-boot-entries
         format-entry-description
         description-to-tree)

;; Describe all entries in a standalone .boot file.
;; Returns a vector of entry descriptions (one per fasl object).
(define (describe-boot-file-entries path)
  (define bv (file->bytes path))
  (call-describe bv))

;; Describe all entries in an embedded boot file within a Racket executable.
;; Reads boot-size bytes starting at section-offset + boot-offset.
(define (describe-exe-boot-entries path section-offset boot-offset boot-size)
  (define bv
    (call-with-input-file path
                          (lambda (in)
                            (file-position in (+ section-offset boot-offset))
                            (read-bytes boot-size in))))
  (call-describe bv))

;; -------------------------------------------------------------------
;; Per-entry describe: scan boot file, describe each fasl entry individually

(define (call-describe bv)
  (define running-ver (get-running-chez-version))
  (define-values (header-bytes entries) (scan-boot-entries bv))
  (define patched-header (patch-header-version header-bytes running-ver))
  (for/vector ([entry (in-list entries)])
    (match entry
      [(list 'fasl entry-bytes)
       ;; Single-entry boot file: header + entry (EOF signals end)
       (define mini-bv (bytes-append patched-header entry-bytes))
       (with-handlers ([exn:fail? (lambda (e) (format "Error: ~a" (exn-message e)))])
         (define result
           (vm-eval `(($primitive $describe-fasl-from-port) (open-bytevector-input-port ,mini-bv)
                                                            '#())))
         (define desc-entries (cadr result))
         ;; desc-entries is (HEADER-desc ENTRY-desc ...), skip the header
         (define entry-descs (and (pair? desc-entries) (cdr desc-entries)))
         (and (pair? entry-descs)
              (let ([desc (car entry-descs)])
                (disassemble-in-description desc)
                desc)))]
      [(list 'vfasl _) #f])))

;; -------------------------------------------------------------------
;; Boot file scanner: extract header bytes and entry byte ranges

(define (scan-boot-entries bv)
  (define in (open-input-bytes bv))
  (define first-header-bytes #f)
  (define entries '())
  (let loop ()
    (define pos (file-position in))
    (define b (read-byte in))
    (cond
      [(eof-object? b) (void)]
      ;; fasl-type-header = 0
      [(= b 0)
       (read-bytes 7 in) ; magic continuation
       (read-uptr* in) ; version
       (read-uptr* in) ; machine-type
       (skip-paren-string in) ; deps
       (define end (file-position in))
       (unless first-header-bytes
         (set! first-header-bytes (subbytes bv pos end)))
       (loop)]
      ;; fasl-type-visit=35, revisit=36, visit-revisit=37
      [(or (= b 35) (= b 36) (= b 37))
       (define size (read-uptr* in))
       (define data-start (file-position in))
       ;; Peek at compression + kind bytes
       (read-byte in) ; comp-byte
       (define kind-byte (read-byte in))
       (define kind (if (= kind-byte 100) 'fasl 'vfasl))
       ;; Seek to end of entry data
       (file-position in (+ data-start size))
       ;; Full entry bytes: type byte through end of data
       (set! entries (cons (list kind (subbytes bv pos (+ data-start size))) entries))
       (loop)]
      ;; fasl-type-terminator = 127
      [(= b 127) (void)]
      [else (void)]))
  (values (or first-header-bytes #"") (reverse entries)))

;; -------------------------------------------------------------------
;; Version patching: replace header version with running Chez version

(define (patch-header-version header-bytes running-ver)
  (cond
    [(< (bytes-length header-bytes) 8) header-bytes]
    [else
     (define in (open-input-bytes header-bytes))
     (define out (open-output-bytes))
     ;; Copy magic (8 bytes)
     (write-bytes (read-bytes 8 in) out)
     ;; Skip old version uptr
     (read-uptr* in)
     ;; Write running version
     (write-uptr* running-ver out)
     ;; Copy rest (machine-type + deps) as-is
     (write-bytes (port->bytes in) out)
     (get-output-bytes out)]))

;; -------------------------------------------------------------------
;; Running Chez version

(define running-chez-version #f)

(define (get-running-chez-version)
  (unless running-chez-version
    ;; Write a minimal fasl and read the version integer from its header.
    ;; This gives the exact internal version that the fasl reader expects,
    ;; including any pre-release field the version string may hide.
    (define bv
      (vm-eval '(let-values ([(o get) (open-bytevector-output-port)])
                  (fasl-write #t o)
                  (get))))
    (define in (open-input-bytes (subbytes bv 8)))
    (set! running-chez-version (read-uptr* in)))
  running-chez-version)

;; -------------------------------------------------------------------
;; uptr codec (variable-length, MSB continuation bit)

(define (read-uptr* in)
  (let loop ([n 0])
    (define b (read-byte in))
    (when (eof-object? b)
      (error 'read-uptr "unexpected EOF"))
    (define n* (bitwise-ior (arithmetic-shift n 7) (bitwise-and b #x7f)))
    (if (bitwise-bit-set? b 7)
        (loop n*)
        n*)))

(define (write-uptr* n out)
  (define groups '())
  (let loop ([n n])
    (set! groups (cons (bitwise-and n #x7F) groups))
    (when (> (arithmetic-shift n -7) 0)
      (loop (arithmetic-shift n -7))))
  (define len (length groups))
  (for ([g (in-list groups)]
        [i (in-naturals)])
    (write-byte (if (< i (sub1 len))
                    (bitwise-ior g #x80)
                    g)
                out)))

(define (skip-paren-string in)
  (read-byte in) ; '('
  (let loop ()
    (define b (read-byte in))
    (unless (or (eof-object? b) (= b (char->integer #\))))
      (loop))))

;; -------------------------------------------------------------------
;; description-to-tree: convert a fasl description into detail strings
;; and expandable child tnodes for CODE objects.
;; Returns (values (listof string?) (listof tnode?))

(define (description-to-tree desc parent-id)
  (cond
    [(not (vector? desc)) (values (format-entry-description desc) '())]
    [(< (vector-length desc) 1) (values (format-entry-description desc) '())]
    [else
     (define tag (vector-ref desc 0))
     (case tag
       [(ENTRY)
        (if (>= (vector-length desc) 3)
            (entry-to-tree (vector-ref desc 2) parent-id)
            (values (format-entry-description desc) '()))]
       [(CODE)
        (code-to-tree desc parent-id)]
       [(CLOSURE)
        (if (>= (vector-length desc) 3)
            (closure-to-tree desc parent-id)
            (values (format-entry-description desc) '()))]
       [else (values (format-entry-description desc) '())])]))

(define (entry-to-tree val parent-id)
  (cond
    [(and (vector? val) (>= (vector-length val) 1))
     (case (vector-ref val 0)
       [(CLOSURE) (closure-to-tree val parent-id)]
       [(CODE) (code-to-tree val parent-id)]
       [else (values (format-entry-description val) '())])]
    [else (values (format-entry-description val) '())]))

(define (closure-to-tree closure parent-id)
  (define offset (vector-ref closure 1))
  (define code (vector-ref closure 2))
  (define-values (code-details code-children)
    (code-to-tree code parent-id))
  (values (cons (format "CLOSURE offset=~a" offset) code-details)
          code-children))

;; Extract assembly lines from a CODE vector's assembly slot (index 7).
(define (code-asm-lines code)
  (define asm (and (>= (vector-length code) 8) (vector-ref code 7)))
  (cond
    [(and (pair? asm) (pair? (car asm)) (eq? (caar asm) '#%assembly-code))
     (cdar asm)]
    [(bytes? asm)
     (list (format "<~a bytes of machine code>" (bytes-length asm)))]
    [else '()]))

;; Extract relocs as a list from a CODE vector's reloc slot (index 9).
(define (code-reloc-list code)
  (define relocs-raw (and (>= (vector-length code) 10) (vector-ref code 9)))
  (cond
    [(vector? relocs-raw) (vector->list relocs-raw)]
    [(list? relocs-raw) relocs-raw]
    [else '()]))

;; Check if a RELOC vector's value is a CODE or CLOSURE containing CODE.
(define (reloc-has-code? r)
  (and (vector? r) (>= (vector-length r) 5) (eq? (vector-ref r 0) 'RELOC)
       (let ([val (vector-ref r 4)])
         (and (vector? val) (>= (vector-length val) 1)
              (memq (vector-ref val 0) '(CODE CLOSURE))))))

;; Symbols that appear in nearly every compiler-generated code object
;; and don't help identify what the code does.
(define boilerplate-symbols
  (list->seteq '($oops compiler-internal coerce-opnd
                 uninitialized fp u uptr)))

;; Infer a name for an unnamed CODE vector from its relocs.
;; Returns a string or #f.
(define (infer-code-name code)
  (define relocs (code-reloc-list code))
  ;; Try: first distinctive symbol reloc
  (or (for/or ([r (in-list relocs)])
        (and (vector? r) (>= (vector-length r) 5) (eq? (vector-ref r 0) 'RELOC)
             (let ([v (vector-ref r 4)])
               (and (symbol? v) (not (set-member? boilerplate-symbols v))
                    (symbol->string v)))))
      ;; Try: first named CODE or CLOSURE reloc
      (for/or ([r (in-list relocs)])
        (and (vector? r) (>= (vector-length r) 5) (eq? (vector-ref r 0) 'RELOC)
             (let ([v (vector-ref r 4)])
               (and (vector? v) (>= (vector-length v) 1)
                    (case (vector-ref v 0)
                      [(CODE)
                       (and (>= (vector-length v) 4) (vector-ref v 3))]
                      [(CLOSURE)
                       (let ([ic (and (>= (vector-length v) 3) (vector-ref v 2))])
                         (and (vector? ic) (>= (vector-length ic) 4) (vector-ref ic 3)))]
                      [else #f])))))))

;; Format a CODE name for display in a label.
;; actual-name: the CODE's own name field (string or #f)
;; code: the CODE vector (for inferring a name if actual-name is #f)
(define (format-code-name actual-name code)
  (cond
    [actual-name (format "~s" actual-name)]
    [else
     (define inferred (infer-code-name code))
     (if inferred
         (format "[~a]" inferred)
         "#f")]))

;; Extract the CODE vector from a reloc value (CODE or CLOSURE wrapping CODE).
(define (extract-code-from-value val)
  (cond
    [(not (vector? val)) #f]
    [(eq? (vector-ref val 0) 'CODE) val]
    [(and (eq? (vector-ref val 0) 'CLOSURE)
          (>= (vector-length val) 3)
          (vector? (vector-ref val 2))
          (>= (vector-length (vector-ref val 2)) 1)
          (eq? (vector-ref (vector-ref val 2) 0) 'CODE))
     (vector-ref val 2)]
    [else #f]))

;; Build summary detail strings + child tnodes from a CODE vector.
;; #(CODE flags free name arity-mask info pinfos assembly size relocs)
(define (code-to-tree code parent-id)
  (define flags (vector-ref code 1))
  (define name (vector-ref code 3))
  (define arity-mask (vector-ref code 4))
  (define asm-lines (code-asm-lines code))
  (define asm-line-count (length asm-lines))
  (define relocs (code-reloc-list code))
  (define total-relocs (length relocs))

  ;; Classify relocs
  (define-values (code-count closure-count)
    (for/fold ([cc 0] [cl 0])
              ([r (in-list relocs)])
      (define val (and (vector? r) (>= (vector-length r) 5)
                       (eq? (vector-ref r 0) 'RELOC)
                       (vector-ref r 4)))
      (cond
        [(and (vector? val) (>= (vector-length val) 1)
              (eq? (vector-ref val 0) 'CODE))
         (values (add1 cc) cl)]
        [(and (vector? val) (>= (vector-length val) 1)
              (eq? (vector-ref val 0) 'CLOSURE))
         (values cc (add1 cl))]
        [else (values cc cl)])))

  ;; Summary lines for parent node
  (define summary
    (list (format "CODE name=~s arity-mask=~a flags=~a" name arity-mask flags)
          (format "~a relocs (~a code, ~a closure)" total-relocs code-count closure-count)))

  ;; Top-level code tnode
  (define top-code-id (string->symbol (format "~a.code" parent-id)))
  (define top-label
    (format "CODE ~a — top-level (~a asm lines, ~a relocs)"
            (format-code-name name code)
            asm-line-count total-relocs))

  ;; Details for top-level code: assembly + non-code relocs
  (define non-code-reloc-lines
    (for/list ([r (in-list relocs)]
               #:when (not (reloc-has-code? r)))
      (format-reloc-line r)))
  (define top-details
    (append asm-lines
            (if (pair? non-code-reloc-lines)
                (cons "" non-code-reloc-lines)
                '())))

  ;; Child tnodes for code/closure objects found in relocs
  (define reloc-children
    (for/list ([r (in-list relocs)]
               [i (in-naturals)]
               #:when (reloc-has-code? r))
      (build-reloc-code-tnode r i parent-id)))

  (define top-child
    (tnode top-code-id top-label top-details '() #t))

  (values summary (cons top-child reloc-children)))

;; Build a tnode for a CODE object found in a reloc.
(define (build-reloc-code-tnode r idx parent-id)
  (define val (vector-ref r 4))
  (define code (extract-code-from-value val))
  (define name (and code (>= (vector-length code) 4) (vector-ref code 3)))
  (define asm-lines (if code (code-asm-lines code) '()))
  (define asm-line-count (length asm-lines))
  (define relocs (if code (code-reloc-list code) '()))
  (define reloc-count (length relocs))

  ;; Format all relocs as text (no further nesting)
  (define reloc-lines
    (for/list ([r (in-list relocs)])
      (format-reloc-line r)))

  (define tnode-id (string->symbol (format "~a.r~a" parent-id idx)))
  (define label
    (format "CODE ~a (~a asm lines, ~a relocs)"
            (format-code-name name code)
            asm-line-count reloc-count))
  (define details
    (append asm-lines
            (if (pair? reloc-lines)
                (cons "" reloc-lines)
                '())))

  (tnode tnode-id label details '() #t))

;; Format a single RELOC as a display line.
(define (format-reloc-line r)
  (cond
    [(and (vector? r) (>= (vector-length r) 5) (eq? (vector-ref r 0) 'RELOC))
     (define type (vector-ref r 1))
     (define offset (vector-ref r 2))
     (define val (vector-ref r 4))
     (define val-str
       (cond
         [(and (vector? val) (>= (vector-length val) 1))
          (define tag (vector-ref val 0))
          (case tag
            [(CODE)
             (define n (and (>= (vector-length val) 4) (vector-ref val 3)))
             (format "CODE ~s" n)]
            [(CLOSURE)
             (define inner-code (and (>= (vector-length val) 3) (vector-ref val 2)))
             (define n (and (vector? inner-code) (>= (vector-length inner-code) 4)
                            (vector-ref inner-code 3)))
             (format "CLOSURE ~s" n)]
            [(LIBRARY LIBRARY-CODE libspec)
             (format "~a" val)]
            [(RECORD)
             (define n (and (>= (vector-length val) 2) (vector-ref val 1)))
             (format "RECORD ~a" n)]
            [(CYCLE)
             (define ref (and (>= (vector-length val) 2) (vector-ref val 1)))
             (define tag* (and (vector? ref) (>= (vector-length ref) 1) (vector-ref ref 0)))
             (if tag* (format "CYCLE(~a)" tag*) "CYCLE")]
            [else (format "~a" val)])]
         [(symbol? val) (format "~a" val)]
         [(string? val) (format "~s" val)]
         [else (format "~a" val)]))
     (format "RELOC ~a@~a → ~a" type offset val-str)]
    [else (format "~a" r)]))

;; -------------------------------------------------------------------
;; Format one entry description into a list of display lines.

(define (format-entry-description desc)
  (cond
    [(not desc) '("(no description)")]
    [(string? desc) (list desc)]
    [else
     (define out (open-output-string))
     (parameterize ([current-output-port out]
                    [pretty-print-columns 100])
       (pretty-print desc))
     (define lines (string-split (get-output-string out) "\n"))
     (if (and (pair? lines) (equal? (last lines) ""))
         (drop-right lines 1)
         lines)]))
