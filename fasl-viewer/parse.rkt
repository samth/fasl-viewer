#lang racket/base

;; parse.rkt — Binary parsing for Chez Scheme fasl/vfasl files,
;; Racket executables with .rackboot sections, and Racket .zo files.

(require racket/match
         racket/list
         racket/port
         racket/file
         file/gunzip)

(provide parse-file
         format-scheme-version
         lookup-machine-type
         lookup-fasl-content-type

         ;; Parsed structures
         (struct-out parsed-file)
         (struct-out chez-boot-file)
         (struct-out boot-header)
         (struct-out fasl-object)
         (struct-out vfasl-header)
         (struct-out racket-executable)
         (struct-out racket-boot-entry)
         (struct-out zo-file)
         (struct-out unknown-file))

;; Top-level result
(struct parsed-file (path format data) #:transparent)

;; Chez boot/fasl file
(struct chez-boot-file (headers objects) #:transparent)

;; File header (one per header in the boot file)
(struct boot-header (version machine-type dependencies) #:transparent)

;; A single fasl object (visit/revisit block)
(struct fasl-object (index situation compression compressed-size
                     uncompressed-size kind vfasl-hdr content-type)
  #:transparent)

;; Parsed vfasl header (14 uptr fields on 64-bit = 112 bytes)
(struct vfasl-header
        (data-size table-size
                   result-offset
                   vspace-offsets ; list of 8 relative offsets
                   symref-count
                   rtdref-count
                   singletonref-count)
  #:transparent)

;; Racket executable with embedded .rackboot
(struct racket-executable
        (section-offset section-size
                        boot-offsets ; list of 4 ints
                        boot-files) ; list of chez-boot-file
  #:transparent)

(struct racket-boot-entry (offset size boot) #:transparent)

;; Racket .zo file
(struct zo-file (version machine-type tag hash) #:transparent)

;; Unknown format
(struct unknown-file (first-bytes) #:transparent)

;; -------------------------------------------------------------------
;; Machine type table (number -> name)
;; Built from cmacros.ss define-machine-types enumeration.

(define machine-type-names
  (make-hasheqv '((#x00 . "any") (#x01 . "pb")
                                 (#x02 . "tpb")
                                 (#x03 . "pb32l")
                                 (#x04 . "tpb32l")
                                 (#x05 . "pb32b")
                                 (#x06 . "tpb32b")
                                 (#x07 . "pb64l")
                                 (#x08 . "tpb64l")
                                 (#x09 . "pb64b")
                                 (#x0A . "tpb64b")
                                 (#x0B . "i3nt")
                                 (#x0C . "ti3nt")
                                 (#x0D . "i3osx")
                                 (#x0E . "ti3osx")
                                 (#x0F . "i3le")
                                 (#x10 . "ti3le")
                                 (#x11 . "i3fb")
                                 (#x12 . "ti3fb")
                                 (#x13 . "i3ob")
                                 (#x14 . "ti3ob")
                                 (#x15 . "i3nb")
                                 (#x16 . "ti3nb")
                                 (#x17 . "i3s2")
                                 (#x18 . "ti3s2")
                                 (#x19 . "i3qnx")
                                 (#x1A . "ti3qnx")
                                 (#x1B . "i3gnu")
                                 (#x1C . "ti3gnu")
                                 (#x1D . "a6gnu")
                                 (#x1E . "ta6gnu")
                                 (#x1F . "a6nt")
                                 (#x20 . "ta6nt")
                                 (#x21 . "a6osx")
                                 (#x22 . "ta6osx")
                                 (#x23 . "a6ios")
                                 (#x24 . "ta6ios")
                                 (#x25 . "a6le")
                                 (#x26 . "ta6le")
                                 (#x27 . "a6fb")
                                 (#x28 . "ta6fb")
                                 (#x29 . "a6ob")
                                 (#x2A . "ta6ob")
                                 (#x2B . "a6nb")
                                 (#x2C . "ta6nb")
                                 (#x2D . "a6s2")
                                 (#x2E . "ta6s2")
                                 (#x2F . "ppc32osx")
                                 (#x30 . "tppc32osx")
                                 (#x31 . "ppc32le")
                                 (#x32 . "tppc32le")
                                 (#x33 . "ppc32fb")
                                 (#x34 . "tppc32fb")
                                 (#x35 . "ppc32ob")
                                 (#x36 . "tppc32ob")
                                 (#x37 . "ppc32nb")
                                 (#x38 . "tppc32nb")
                                 (#x39 . "arm32le")
                                 (#x3A . "tarm32le")
                                 (#x3B . "arm32fb")
                                 (#x3C . "tarm32fb")
                                 (#x3D . "arm32ob")
                                 (#x3E . "tarm32ob")
                                 (#x3F . "arm32nb")
                                 (#x40 . "tarm32nb")
                                 (#x41 . "arm64nt")
                                 (#x42 . "tarm64nt")
                                 (#x43 . "arm64osx")
                                 (#x44 . "tarm64osx")
                                 (#x45 . "arm64ios")
                                 (#x46 . "tarm64ios")
                                 (#x47 . "arm64le")
                                 (#x48 . "tarm64le")
                                 (#x49 . "arm64fb")
                                 (#x4A . "tarm64fb")
                                 (#x4B . "arm64ob")
                                 (#x4C . "tarm64ob")
                                 (#x4D . "arm64nb")
                                 (#x4E . "tarm64nb")
                                 (#x4F . "rv64le")
                                 (#x50 . "trv64le")
                                 (#x51 . "rv64fb")
                                 (#x52 . "trv64fb")
                                 (#x53 . "rv64ob")
                                 (#x54 . "trv64ob")
                                 (#x55 . "rv64nb")
                                 (#x56 . "trv64nb")
                                 (#x57 . "la64le")
                                 (#x58 . "tla64le"))))

(define (lookup-machine-type n)
  (hash-ref machine-type-names n (lambda () (format "unknown(~a)" n))))

;; -------------------------------------------------------------------
;; Version formatting (matches S_format_scheme_version in fasl.c)

(define (format-scheme-version n)
  (define major (arithmetic-shift n -24))
  (define minor (bitwise-and (arithmetic-shift n -16) #xff))
  (define patch (bitwise-and (arithmetic-shift n -8) #xff))
  (define pre (bitwise-and n #xff))
  (if (zero? pre)
      (format "~a.~a.~a" major minor patch)
      (format "~a.~a.~a-pre-release.~a" major minor patch pre)))

;; -------------------------------------------------------------------
;; Fasl content type names (the tag byte at start of fasl data)

(define fasl-content-type-names
  (make-hasheqv
   '((0 . "header") (1 . "box") (2 . "symbol") (3 . "ratnum")
     (4 . "vector") (5 . "inexactnum") (6 . "closure") (7 . "pair")
     (8 . "flonum") (9 . "string") (10 . "bignum") (11 . "code")
     (12 . "immediate") (13 . "entry") (14 . "library")
     (15 . "library-code") (16 . "graph") (17 . "graph-def")
     (18 . "graph-ref") (19 . "gensym") (20 . "exactnum")
     (21 . "uninterned-symbol") (22 . "stencil-vector")
     (23 . "system-stencil-vector") (24 . "record") (25 . "rtd")
     (26 . "small-integer") (27 . "base-rtd") (28 . "fxvector")
     (29 . "ephemeron") (30 . "bytevector") (31 . "weak-pair")
     (32 . "eq-hashtable") (33 . "symbol-hashtable") (34 . "phantom")
     (38 . "immutable-vector") (39 . "immutable-string")
     (40 . "flvector") (41 . "immutable-bytevector")
     (42 . "immutable-box") (43 . "begin"))))

(define (lookup-fasl-content-type n)
  (hash-ref fasl-content-type-names n (lambda () (format "tag-~a" n))))

;; Peek at first byte of LZ4-compressed data without full decompression
(define (lz4-peek-first-byte data)
  (and (>= (bytes-length data) 2)
       (let* ([token (bytes-ref data 0)]
              [lit-len (arithmetic-shift token -4)])
         (cond
           [(zero? lit-len) #f]
           [(< lit-len 15) (bytes-ref data 1)]
           [else
            (let loop ([i 1])
              (and (< i (bytes-length data))
                   (if (= (bytes-ref data i) 255)
                       (loop (add1 i))
                       (and (< (add1 i) (bytes-length data))
                            (bytes-ref data (add1 i))))))]))))

;; -------------------------------------------------------------------
;; Determine pointer size from machine type.
;; 32-bit types: i3*, ppc32*, arm32*, pb32*, pb (generic)
;; Everything else: 64-bit

(define (machine-type-ptr-bytes mt-num)
  (define name (lookup-machine-type mt-num))
  (cond
    [(regexp-match? #rx"^t?i3" name) 4]
    [(regexp-match? #rx"^t?ppc32" name) 4]
    [(regexp-match? #rx"^t?arm32" name) 4]
    [(regexp-match? #rx"^t?pb32" name) 4]
    ;; Generic pb is ambiguous; assume 64-bit
    [else 8]))

;; -------------------------------------------------------------------
;; uptr decoding (variable-length, MSB continuation bit)
;; See S_fasl_uptrin in fasl.c:705

(define (read-uptr in)
  (let loop ([n 0])
    (define b (read-byte in))
    (when (eof-object? b)
      (error 'read-uptr "unexpected EOF"))
    (define n* (bitwise-ior (arithmetic-shift n 7) (bitwise-and b #x7f)))
    (if (bitwise-bit-set? b 7)
        (loop n*)
        n*)))

;; -------------------------------------------------------------------
;; Top-level dispatcher

(define (parse-file path)
  (call-with-input-file
   path
   (lambda (in)
     (define magic (read-bytes 8 in))
     (file-position in 0)
     (cond
       ;; Chez fasl/boot: starts with 00 00 00 00 63 68 65 7a
       [(and (bytes? magic)
             (>= (bytes-length magic) 8)
             (= (bytes-ref magic 0) 0)
             (= (bytes-ref magic 1) 0)
             (= (bytes-ref magic 2) 0)
             (= (bytes-ref magic 3) 0)
             (= (bytes-ref magic 4) (char->integer #\c))
             (= (bytes-ref magic 5) (char->integer #\h))
             (= (bytes-ref magic 6) (char->integer #\e))
             (= (bytes-ref magic 7) (char->integer #\z)))
        (parsed-file path 'chez-boot (parse-chez-boot in))]
       ;; Racket .zo: starts with #~ (0x23 0x7e)
       [(and (bytes? magic)
             (>= (bytes-length magic) 2)
             (= (bytes-ref magic 0) #x23)
             (= (bytes-ref magic 1) #x7e))
        (parsed-file path 'zo (parse-zo in))]
       ;; ELF: 7f 45 4c 46
       [(and (bytes? magic)
             (>= (bytes-length magic) 4)
             (= (bytes-ref magic 0) #x7f)
             (= (bytes-ref magic 1) (char->integer #\E))
             (= (bytes-ref magic 2) (char->integer #\L))
             (= (bytes-ref magic 3) (char->integer #\F)))
        (parsed-file path 'racket-executable (parse-elf-rackboot in path))]
       [else (parsed-file path 'unknown (unknown-file (if (bytes? magic) magic #"")))]))))

;; -------------------------------------------------------------------
;; Chez boot/fasl file parser

(define (parse-chez-boot in)
  (define headers '())
  (define objects '())
  (define obj-index 0)
  (define current-mt 0) ; last-seen machine type from header

  (let loop ()
    (define b (read-byte in))
    (cond
      [(eof-object? b) (void)]
      ;; fasl-type-header = 0
      [(= b 0)
       ;; Read rest of header magic: 00 00 00 'c' 'h' 'e' 'z'
       (define hdr-bytes (read-bytes 7 in))
       (unless (and (bytes? hdr-bytes)
                    (= (bytes-length hdr-bytes) 7)
                    (= (bytes-ref hdr-bytes 0) 0)
                    (= (bytes-ref hdr-bytes 1) 0)
                    (= (bytes-ref hdr-bytes 2) 0)
                    (= (bytes-ref hdr-bytes 3) (char->integer #\c))
                    (= (bytes-ref hdr-bytes 4) (char->integer #\h))
                    (= (bytes-ref hdr-bytes 5) (char->integer #\e))
                    (= (bytes-ref hdr-bytes 6) (char->integer #\z)))
         (error 'parse-chez-boot "bad header magic"))
       ;; Version (uptr)
       (define version (read-uptr in))
       ;; Machine type (uptr)
       (set! current-mt (read-uptr in))
       ;; Dependencies in parens
       (define deps (read-paren-string in))
       (set! headers (cons (boot-header version current-mt deps) headers))
       (loop)]
      ;; fasl-type-visit=35, revisit=36, visit-revisit=37
      [(or (= b 35) (= b 36) (= b 37))
       (define situation
         (case b
           [(35) 'visit]
           [(36) 'revisit]
           [(37) 'visit-revisit]))
       (define size (read-uptr in))
       (define start-pos (file-position in))
       ;; compression byte
       (define comp-byte (read-byte in))
       ;; kind byte (fasl=100, vfasl=101)
       (define kind-byte (read-byte in))

       (define compression
         (case comp-byte
           [(44) 'uncompressed]
           [(45) 'gzip]
           [(46) 'lz4]
           [else (format "unknown(~a)" comp-byte)]))

       (define kind
         (case kind-byte
           [(100) 'fasl]
           [(101) 'vfasl]
           [else (format "unknown(~a)" kind-byte)]))

       (define-values (compressed-size uncompressed-size vhdr ctype)
         (case comp-byte
           [(45 46) ; gzip or lz4
            (define usize (read-uptr in))
            (define bytes-read-so-far (- (file-position in) start-pos))
            (define csize (- size bytes-read-so-far))
            (cond
              [(= kind-byte 101) ; vfasl
               (define vh (parse-vfasl-header-from-compressed
                           in comp-byte csize usize current-mt))
               (values csize usize vh #f)]
              [else
               ;; For fasl, peek at first content byte from compressed data
               (define save-pos (file-position in))
               (define compressed-data (read-bytes csize in))
               (file-position in save-pos)
               (define ct
                 (and (bytes? compressed-data)
                      (= comp-byte 46) ; LZ4 only
                      (lz4-peek-first-byte compressed-data)))
               (values csize usize #f ct)])]
           [(44) ; uncompressed
            (define remaining (- size 2))
            (cond
              [(= kind-byte 101)
               (define vh (parse-vfasl-header-inline in current-mt))
               (values remaining remaining vh #f)]
              [else
               ;; Peek at the first byte of fasl content
               (define ct (read-byte in))
               (values remaining remaining #f
                       (if (eof-object? ct) #f ct))])]
           [else (values (- size 2) (- size 2) #f #f)]))

       (set!
        objects
        (cons
         (fasl-object obj-index situation compression
                      compressed-size uncompressed-size kind vhdr ctype)
         objects))
       (set! obj-index (add1 obj-index))
       ;; Skip to end of this object
       (file-position in (+ start-pos size))
       (loop)]
      ;; fasl-type-terminator = 127
      [(= b 127) (void)]
      ;; Unknown byte, stop
      [else (void)]))

  (chez-boot-file (reverse headers) (reverse objects)))

;; Read a parenthesized dependency string like "(petite)" or "()"
(define (read-paren-string in)
  (define open (read-byte in))
  (unless (and (not (eof-object? open)) (= open (char->integer #\()))
    (error 'read-paren-string "expected '('"))
  (define chars
    (let loop ([acc '()])
      (define b (read-byte in))
      (cond
        [(eof-object? b) (error 'read-paren-string "unexpected EOF")]
        [(= b (char->integer #\))) (reverse acc)]
        [else (loop (cons (integer->char b) acc))])))
  (list->string chars))

;; Parse a vfasl header from uncompressed inline data.
;; Reads ptr-size * 14 bytes (or actually: data-size, table-size,
;; result-offset, 8 vspace-rel-offsets, symref-count, rtdref-count,
;; singletonref-count = 14 fields total but field 4 is an array of 8).
;; That's 3 + 8 + 3 = 14 uptr fields.
(define (parse-vfasl-header-inline in mt)
  (define ptr-size (machine-type-ptr-bytes mt))
  (define (read-uptr-fixed)
    (define bs (read-bytes ptr-size in))
    (when (or (not (bytes? bs)) (< (bytes-length bs) ptr-size))
      (error 'parse-vfasl-header "unexpected EOF reading vfasl header"))
    (integer-bytes->integer bs #f #f)) ; unsigned, little-endian
  (define data-size (read-uptr-fixed))
  (define table-size (read-uptr-fixed))
  (define result-offset (read-uptr-fixed))
  (define vspace-offsets
    (for/list ([_ (in-range 8)])
      (read-uptr-fixed)))
  (define symref-count (read-uptr-fixed))
  (define rtdref-count (read-uptr-fixed))
  (define singletonref-count (read-uptr-fixed))
  (vfasl-header data-size
                table-size
                result-offset
                vspace-offsets
                symref-count
                rtdref-count
                singletonref-count))

;; Parse vfasl header from compressed data — decompress first, then read header.
(define (parse-vfasl-header-from-compressed in comp-byte csize usize mt)
  (define compressed-data (read-bytes csize in))
  (when (or (not (bytes? compressed-data)) (< (bytes-length compressed-data) csize))
    (error 'parse-vfasl-header "unexpected EOF reading compressed data"))
  (define decompressed
    (cond
      ; LZ4
      [(= comp-byte 46) (lz4-decompress compressed-data usize)]
      ; gzip
      [(= comp-byte 45) (gzip-decompress compressed-data)]
      [else compressed-data]))
  (define ptr-size (machine-type-ptr-bytes mt))
  (define header-size (* 14 ptr-size))
  (when (< (bytes-length decompressed) header-size)
    (error 'parse-vfasl-header "decompressed data too small for vfasl header"))
  (define hin (open-input-bytes decompressed))
  (parse-vfasl-header-inline hin mt))

;; -------------------------------------------------------------------
;; LZ4 decompression (minimal block format)

(define (lz4-decompress src dest-size)
  (define out (make-bytes dest-size))
  (define slen (bytes-length src))
  (let loop ([si 0]
             [di 0])
    (when (< si slen)
      (define token (bytes-ref src si))
      (define lit-len (arithmetic-shift token -4))
      (define match-len (bitwise-and token #xf))
      (define si* (add1 si))
      ;; Extended literal length
      (define-values (lit-len* si2)
        (if (= lit-len 15)
            (let ext ([l lit-len]
                      [s si*])
              (define b (bytes-ref src s))
              (define l* (+ l b))
              (if (= b 255)
                  (ext l* (add1 s))
                  (values l* (add1 s))))
            (values lit-len si*)))
      ;; Copy literals
      (bytes-copy! out di src si2 (+ si2 lit-len*))
      (define di2 (+ di lit-len*))
      (define si3 (+ si2 lit-len*))
      (cond
        [(>= si3 slen) (void)] ; done, last sequence has no match
        [else
         ;; Read 16-bit little-endian offset
         (define offset (+ (bytes-ref src si3) (arithmetic-shift (bytes-ref src (add1 si3)) 8)))
         (define si4 (+ si3 2))
         ;; Extended match length
         (define-values (match-len* si5)
           (if (= match-len 15)
               (let ext ([l (+ match-len 4)]
                         [s si4])
                 (define b (bytes-ref src s))
                 (define l* (+ l b))
                 (if (= b 255)
                     (ext l* (add1 s))
                     (values l* (add1 s))))
               (values (+ match-len 4) si4)))
         ;; Copy match (may overlap, so byte-by-byte)
         (define match-start (- di2 offset))
         (for ([i (in-range match-len*)])
           (bytes-set! out (+ di2 i) (bytes-ref out (+ match-start i))))
         (loop si5 (+ di2 match-len*))])))
  out)

;; gzip decompression using Racket's file/gunzip (raw deflate)
(define (gzip-decompress data)
  (define in (open-input-bytes data))
  (define out (open-output-bytes))
  (inflate in out)
  (get-output-bytes out))

;; -------------------------------------------------------------------
;; ELF parsing for .rackboot section

(define (parse-elf-rackboot in path)
  ;; Read ELF header
  (file-position in 0)
  (define e-ident (read-bytes 16 in))
  (unless (and (bytes? e-ident)
               (= (bytes-length e-ident) 16)
               (= (bytes-ref e-ident 0) #x7f)
               (= (bytes-ref e-ident 1) (char->integer #\E))
               (= (bytes-ref e-ident 2) (char->integer #\L))
               (= (bytes-ref e-ident 3) (char->integer #\F)))
    (error 'parse-elf "not a valid ELF file"))

  (define class (bytes-ref e-ident 4)) ; 1=32-bit, 2=64-bit
  (define data-encoding (bytes-ref e-ident 5)) ; 1=LE, 2=BE
  (define big-endian? (= data-encoding 2))
  (define bits64? (= class 2))

  (define (read-u16)
    (integer-bytes->integer (read-bytes 2 in) #f big-endian?))
  (define (read-u32)
    (integer-bytes->integer (read-bytes 4 in) #f big-endian?))
  (define (read-u64)
    (integer-bytes->integer (read-bytes 8 in) #f big-endian?))
  (define (read-addr)
    (if bits64?
        (read-u64)
        (read-u32)))
  (define (read-off)
    (if bits64?
        (read-u64)
        (read-u32)))
  (define (read-xword)
    (if bits64?
        (read-u64)
        (read-u32)))

  ;; Rest of ELF header
  (read-u16) ; e_type
  (read-u16) ; e_machine
  (read-u32) ; e_version
  (read-addr) ; e_entry
  (read-off) ; e_phoff
  (define e-shoff (read-off)) ; e_shoff
  (read-u32) ; e_flags
  (read-u16) ; e_ehsize
  (read-u16) ; e_phentsize
  (read-u16) ; e_phnum
  (define e-shentsize (read-u16))
  (define e-shnum (read-u16))
  (define e-shstrndx (read-u16))

  ;; Read section header string table
  (define shstrtab-offset
    (begin
      (file-position in (+ e-shoff (* e-shstrndx e-shentsize)))
      ;; Skip to sh_offset field
      (read-u32) ; sh_name
      (read-u32) ; sh_type
      (read-xword) ; sh_flags
      (read-addr) ; sh_addr
      (read-off))) ; sh_offset
  (define shstrtab-size
    (begin
      (read-xword))) ; sh_size

  (file-position in shstrtab-offset)
  (define shstrtab (read-bytes shstrtab-size in))

  ;; Search for .rackboot section
  (define rackboot-info
    (for/or ([i (in-range e-shnum)])
      (file-position in (+ e-shoff (* i e-shentsize)))
      (define sh-name-idx (read-u32))
      (define sh-type (read-u32))
      (define sh-flags (read-xword))
      (define sh-addr (read-addr))
      (define sh-offset (read-off))
      (define sh-size (read-xword))
      (define name (read-cstring shstrtab sh-name-idx))
      (and (equal? name ".rackboot") (list sh-offset sh-size))))

  (unless rackboot-info
    (error 'parse-elf "no .rackboot section found in ~a" path))

  (define section-offset (first rackboot-info))
  (define section-size (second rackboot-info))

  ;; Search for boot offset marker in the ELF file
  ;; The marker "BooT FilE OffsetS:" is in a data section
  (file-position in 0)
  (define file-data (read-bytes (file-size path) in))
  (define marker #"BooT FilE OffsetS:")
  (define marker-pos (bytes-find file-data marker))

  (define boot-offsets
    (if marker-pos
        (let ([base (+ marker-pos 18)])
          (for/list ([i (in-range 4)])
            (integer-bytes->integer file-data #t big-endian? (+ base (* i 4)) (+ base (* i 4) 4))))
        '(0 0 0 0)))

  ;; Parse each embedded boot file
  (define boot-files
    (if (and marker-pos (not (andmap zero? boot-offsets)))
        (let ([offsets (append boot-offsets (list section-size))])
          (for/list ([i (in-range 3)]
                     #:when (< (list-ref offsets i) section-size))
            (define start (list-ref offsets i))
            (define end (list-ref offsets (add1 i)))
            (define boot-size (- end start))
            ;; Position into the .rackboot section
            (file-position in (+ section-offset start))
            ;; Read boot data
            (define boot-data (read-bytes boot-size in))
            (define boot-in (open-input-bytes boot-data))
            ;; Check for terminator byte at end
            (define actual-size
              (if (and (> boot-size 0) (= (bytes-ref boot-data (sub1 boot-size)) #x7f))
                  (sub1 boot-size)
                  boot-size))
            (define trimmed (subbytes boot-data 0 actual-size))
            (define trimmed-in (open-input-bytes trimmed))
            (racket-boot-entry start
                               boot-size
                               (with-handlers ([exn:fail? (lambda (e) (chez-boot-file '() '()))])
                                 (parse-chez-boot trimmed-in)))))
        '()))

  (racket-executable section-offset section-size boot-offsets boot-files))

(define (read-cstring bs offset)
  (define len (bytes-length bs))
  (let loop ([i offset]
             [acc '()])
    (cond
      [(>= i len) (list->string (reverse acc))]
      [(zero? (bytes-ref bs i)) (list->string (reverse acc))]
      [else (loop (add1 i) (cons (integer->char (bytes-ref bs i)) acc))])))

(define (bytes-find haystack needle)
  (define nlen (bytes-length needle))
  (define hlen (bytes-length haystack))
  (for/or ([i (in-range (- hlen nlen -1))])
    (and (for/and ([j (in-range nlen)])
           (= (bytes-ref haystack (+ i j)) (bytes-ref needle j)))
         i)))

;; -------------------------------------------------------------------
;; Racket .zo file parser (header only)

(define (parse-zo in)
  (file-position in 0)
  ;; Skip #~ (2 bytes)
  (read-bytes 2 in)
  ;; Version string: length byte followed by chars
  (define vlen (read-byte in))
  (define version-str
    (if (eof-object? vlen)
        ""
        (bytes->string/utf-8 (read-bytes vlen in))))
  ;; Machine type: length byte followed by chars
  (define mtlen (read-byte in))
  (define machine-type-str
    (if (eof-object? mtlen)
        ""
        (bytes->string/utf-8 (read-bytes mtlen in))))
  ;; Tag byte: B=bundle, D=directory, T=linklet
  (define tag-byte (read-byte in))
  (define tag
    (cond
      [(eof-object? tag-byte) "?"]
      [(= tag-byte (char->integer #\B)) "B (bundle)"]
      [(= tag-byte (char->integer #\D)) "D (directory)"]
      [(= tag-byte (char->integer #\T)) "T (linklet)"]
      [else (format "~a" (integer->char tag-byte))]))
  (zo-file version-str machine-type-str tag ""))
