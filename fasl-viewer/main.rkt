#lang racket/base

;; main.rkt — Interactive TUI viewer for Chez Scheme fasl/vfasl files.

(require racket/cmdline
         racket/match
         racket/list
         racket/set
         racket/string
         kettle
         kettle/run
         "parse.rkt"
         "render.rkt"
         "describe.rkt")

(provide make-app
         app-on-key
         app-on-msg
         app-to-view
         (struct-out app)
         find-tnode-children)

;; Application state
(struct app
        (tree ; list of tnode (root nodes)
         items ; list of flat-item (visible lines)
         cursor ; integer index into items
         expanded ; set of node ids that are expanded
         width ; terminal width
         height ; terminal height
         scroll ; first visible line index
         file-path ; original file path
         search-mode? ; #t when typing a search query
         search-query ; current search string
         search-origin ; cursor position before search started
         parsed ; parsed-file struct
         describe-cache ; (hasheq key -> (vectorof any/c))
         descriptions ; (hasheq symbol? -> (listof string?))
         base-details ; (hasheq symbol? -> (listof string?))
         base-children) ; (hasheq symbol? -> (listof tnode?))
  #:transparent)

(define (make-app pf)
  (define tree (build-tree pf))
  (define expanded (seteq)) ; start collapsed
  (define items (flatten-tree tree expanded))
  (app tree items 0 expanded 80 24 0 (parsed-file-path pf) #f "" 0
       pf (hasheq) (hasheq) (hasheq) (hasheq)))

(define (rebuild-items a)
  (define items (flatten-tree (app-tree a) (app-expanded a)))
  (define cursor (min (app-cursor a) (max 0 (sub1 (length items)))))
  (struct-copy app a [items items] [cursor cursor]))

(define (ensure-visible a)
  (define content-height (- (app-height a) 2))
  (define cursor (app-cursor a))
  (define scroll (app-scroll a))
  (cond
    [(< cursor scroll) (struct-copy app a [scroll cursor])]
    [(>= cursor (+ scroll content-height)) (struct-copy app a [scroll (- cursor content-height -1)])]
    [else a]))

(define (move-cursor a delta)
  (define new-cursor (max 0 (min (sub1 (length (app-items a))) (+ (app-cursor a) delta))))
  (ensure-visible (struct-copy app a [cursor new-cursor])))

(define (toggle-expand a)
  (define items (app-items a))
  (define cursor (app-cursor a))
  (when (>= cursor (length items))
    (void))
  (define item (list-ref items cursor))
  (match item
    [(flat-item id _ _ expandable? expanded? _)
     (cond
       [(not expandable?) a]
       [expanded?
        (define new-exp (set-remove (app-expanded a) id))
        (ensure-visible (rebuild-items (struct-copy app a [expanded new-exp])))]
       [else
        (define new-exp (set-add (app-expanded a) id))
        (ensure-visible (rebuild-items (struct-copy app a [expanded new-exp])))])]))

(define (expand-current a)
  (define items (app-items a))
  (define cursor (app-cursor a))
  (when (>= cursor (length items))
    (void))
  (define item (list-ref items cursor))
  (match item
    [(flat-item id _ _ expandable? expanded? _)
     (cond
       [(and expandable? (not expanded?))
        (define new-exp (set-add (app-expanded a) id))
        (ensure-visible (rebuild-items (struct-copy app a [expanded new-exp])))]
       ;; If already expanded, move to first child
       [(and expandable? expanded? (< (add1 cursor) (length items))) (move-cursor a 1)]
       [else a])]))

(define (collapse-current a)
  (define items (app-items a))
  (define cursor (app-cursor a))
  (define item (list-ref items cursor))
  (match item
    [(flat-item id _ _ expandable? expanded? _)
     (cond
       [(and expandable? expanded?)
        (define new-exp (set-remove (app-expanded a) id))
        (ensure-visible (rebuild-items (struct-copy app a [expanded new-exp])))]
       ;; Move to parent (previous item with lower depth)
       [else
        (define cur-depth (flat-item-depth item))
        (define parent-idx
          (for/or ([i (in-range (sub1 cursor) -1 -1)])
            (and (< (flat-item-depth (list-ref items i)) cur-depth) i)))
        (if parent-idx
            (ensure-visible (struct-copy app a [cursor parent-idx]))
            a)])]))

(define (goto-top a)
  (ensure-visible (struct-copy app a [cursor 0] [scroll 0])))

(define (goto-bottom a)
  (define last-idx (max 0 (sub1 (length (app-items a)))))
  (ensure-visible (struct-copy app a [cursor last-idx])))

(define (page-down a)
  (move-cursor a (- (app-height a) 3)))

(define (page-up a)
  (move-cursor a (- 3 (app-height a))))

(define (resize a w h)
  (ensure-visible (struct-copy app a [width w] [height h])))

;; -------------------------------------------------------------------
;; Search

(define (find-match-from a start direction)
  (define items (app-items a))
  (define query (string-downcase (app-search-query a)))
  (define n (length items))
  (and (not (string=? query ""))
       (> n 0)
       (for/or ([offset (in-range 0 n)])
         (define idx (modulo (+ start (* offset direction)) n))
         (and (string-contains? (string-downcase (flat-item-label (list-ref items idx))) query)
              idx))))

(define (search-jump a)
  (define idx (find-match-from a (app-search-origin a) 1))
  (if idx
      (ensure-visible (struct-copy app a [cursor idx]))
      a))

(define (find-next a)
  (define idx (find-match-from a (add1 (app-cursor a)) 1))
  (if idx
      (ensure-visible (struct-copy app a [cursor idx]))
      a))

(define (find-prev a)
  (define idx (find-match-from a (sub1 (app-cursor a)) -1))
  (if idx
      (ensure-visible (struct-copy app a [cursor idx]))
      a))

(define (search-on-key a msg)
  (match msg
    [(key-msg 'escape _ _)
     ;; Cancel: restore cursor
     (ensure-visible
      (struct-copy app a
                   [search-mode? #f]
                   [search-query ""]
                   [cursor (app-search-origin a)]))]
    [(key-msg (or 'enter #\return) _ _)
     ;; Confirm search
     (struct-copy app a [search-mode? #f])]
    [(key-msg 'backspace _ _)
     (define q (app-search-query a))
     (define new-q (if (> (string-length q) 0)
                       (substring q 0 (sub1 (string-length q)))
                       q))
     (search-jump (struct-copy app a [search-query new-q]))]
    [(key-msg (? char? c) _ _)
     #:when (not (char-control? c))
     (define new-q (string-append (app-search-query a) (string c)))
     (search-jump (struct-copy app a [search-query new-q]))]
    [_ a]))

(define (char-control? c)
  (< (char->integer c) 32))

;; -------------------------------------------------------------------
;; Describe (d key) — deep fasl inspection

(define (describe-current a)
  (define items (app-items a))
  (define cursor (app-cursor a))
  (when (>= cursor (length items)) (void))
  (define item (list-ref items cursor))
  (define id (flat-item-id item))
  (cond
    [(not id) a]
    [else
     (define id-str (symbol->string id))
     (define m (regexp-match #rx"^(?:boot([0-9]+)/)?o([0-9]+)$" id-str))
     (cond
       [(not m) a]
       [else
        (define boot-idx (and (second m) (string->number (second m))))
        (define obj-idx (string->number (third m)))
        ;; Check that this is a fasl (not vfasl) object
        (define pf-data (parsed-file-data (app-parsed a)))
        (define objects
          (cond
            [boot-idx
             (define entry (list-ref (racket-executable-boot-files pf-data) boot-idx))
             (chez-boot-file-objects (racket-boot-entry-boot entry))]
            [else
             (chez-boot-file-objects pf-data)]))
        (cond
          [(>= obj-idx (length objects)) a]
          [(not (memq (fasl-object-kind (list-ref objects obj-idx)) '(fasl vfasl))) a]
          [(hash-has-key? (app-descriptions a) id)
           (toggle-description-off a id)]
          [else
           (toggle-description-on a id boot-idx obj-idx)])])]))

(define (toggle-description-on a id boot-idx obj-idx)
  (define cache-key boot-idx)
  (define cache (app-describe-cache a))
  (define-values (new-cache desc)
    (cond
      [(hash-ref cache cache-key #f)
       => (lambda (entries)
            (values cache (and (< obj-idx (vector-length entries))
                               (vector-ref entries obj-idx))))]
      [else
       (with-handlers
         ([exn:fail?
           (lambda (e)
             (define err-vec (vector (format "Error: ~a" (exn-message e))))
             (values (hash-set cache cache-key err-vec)
                     (format "Error: ~a" (exn-message e))))])
         (define entries (load-descriptions a boot-idx))
         (define new-c (hash-set cache cache-key entries))
         (values new-c (and (< obj-idx (vector-length entries))
                            (vector-ref entries obj-idx))))]))
  (define-values (desc-lines desc-children) (description-to-tree desc id))
  ;; Save original details if not already saved
  (define base (or (find-tnode-details (app-tree a) id) '()))
  (define new-base-details
    (if (hash-has-key? (app-base-details a) id)
        (app-base-details a)
        (hash-set (app-base-details a) id base)))
  (define actual-base (hash-ref new-base-details id base))
  (define new-details (append actual-base (if (pair? actual-base) (list "") '()) desc-lines))
  ;; Save original children
  (define base-ch (or (find-tnode-children (app-tree a) id) '()))
  (define new-base-children
    (if (hash-has-key? (app-base-children a) id)
        (app-base-children a)
        (hash-set (app-base-children a) id base-ch)))
  (define orig-ch (hash-ref new-base-children id base-ch))
  (define new-children (append orig-ch desc-children))
  (define new-tree (update-tnode-in-tree (app-tree a) id new-details new-children))
  (define new-expanded (set-add (app-expanded a) id))
  (define new-descriptions (hash-set (app-descriptions a) id desc-lines))
  (ensure-visible
   (rebuild-items
    (struct-copy app a
                 [tree new-tree]
                 [expanded new-expanded]
                 [describe-cache new-cache]
                 [descriptions new-descriptions]
                 [base-details new-base-details]
                 [base-children new-base-children]))))

(define (toggle-description-off a id)
  (define base-lines (hash-ref (app-base-details a) id '()))
  (define base-ch (hash-ref (app-base-children a) id '()))
  (define new-tree (update-tnode-in-tree (app-tree a) id base-lines base-ch))
  (define new-descriptions (hash-remove (app-descriptions a) id))
  ;; Remove desc children from expanded set
  (define id-prefix (format "~a." id))
  (define new-expanded
    (let ([exp (if (null? base-lines)
                   (set-remove (app-expanded a) id)
                   (app-expanded a))])
      (for/fold ([e exp]) ([eid (in-set exp)])
        (if (string-prefix? (symbol->string eid) id-prefix)
            (set-remove e eid)
            e))))
  (ensure-visible
   (rebuild-items
    (struct-copy app a
                 [tree new-tree]
                 [expanded new-expanded]
                 [descriptions new-descriptions]))))

(define (load-descriptions a boot-idx)
  (define pf-data (parsed-file-data (app-parsed a)))
  (cond
    [boot-idx
     (define exe pf-data)
     (define entry (list-ref (racket-executable-boot-files exe) boot-idx))
     (describe-exe-boot-entries (app-file-path a)
                                (racket-executable-section-offset exe)
                                (racket-boot-entry-offset entry)
                                (racket-boot-entry-size entry))]
    [else
     (describe-boot-file-entries (app-file-path a))]))

(define (update-tnode-in-tree nodes target-id new-details [new-children #f])
  (for/list ([n (in-list nodes)])
    (if (eq? (tnode-id n) target-id)
        (let ([children (or new-children (tnode-children n))])
          (struct-copy tnode n
            [details new-details]
            [children children]
            [expandable? (or (pair? new-details) (pair? children))]))
        (struct-copy tnode n
          [children (update-tnode-in-tree (tnode-children n) target-id new-details new-children)]))))

(define (find-tnode-details nodes id)
  (for/or ([n (in-list nodes)])
    (cond
      [(eq? (tnode-id n) id) (tnode-details n)]
      [else (find-tnode-details (tnode-children n) id)])))

(define (find-tnode-children nodes id)
  (for/or ([n (in-list nodes)])
    (cond
      [(eq? (tnode-id n) id) (tnode-children n)]
      [else (find-tnode-children (tnode-children n) id)])))

;; -------------------------------------------------------------------
;; Key dispatch

(define (app-on-key a msg)
  (cond
    [(app-search-mode? a) (search-on-key a msg)]
    [else
     (match msg
       ;; Quit
       [(key-msg #\q _ _) (cmd a (quit-cmd))]
       ;; Search
       [(key-msg #\/ _ _)
        (struct-copy app a
                     [search-mode? #t]
                     [search-query ""]
                     [search-origin (app-cursor a)])]
       ;; Next/prev match
       [(key-msg #\n _ _) (find-next a)]
       [(key-msg (or #\N #\p) _ _) (find-prev a)]
       ;; Move down
       [(key-msg (or #\j 'down) _ _) (move-cursor a 1)]
       ;; Move up
       [(key-msg (or #\k 'up) _ _) (move-cursor a -1)]
       ;; Toggle expand
       [(key-msg (or 'enter #\return #\space) _ _) (toggle-expand a)]
       ;; Expand / move into
       [(key-msg (or #\l 'right) _ _) (expand-current a)]
       ;; Collapse / move to parent
       [(key-msg (or #\h 'left) _ _) (collapse-current a)]
       ;; Top / bottom
       [(key-msg #\g _ _) (goto-top a)]
       [(key-msg #\G _ _) (goto-bottom a)]
       ;; Describe fasl entry
       [(key-msg #\d _ #f) (describe-current a)]
       ;; Page down/up
       [(key-msg 'page-down _ _) (page-down a)]
       [(key-msg 'page-up _ _) (page-up a)]
       [(key-msg #\d _ #t) (page-down a)] ; ctrl-d
       [(key-msg #\u _ #t) (page-up a)] ; ctrl-u
       [_ a])]))

(define (app-on-msg a msg)
  (match msg
    [(window-size-msg w h) (resize a w h)]
    [_ a]))

(define (cursor-describable? a)
  (define items (app-items a))
  (define cursor (app-cursor a))
  (and (< cursor (length items))
       (let ([id (flat-item-id (list-ref items cursor))])
         (and id
              (regexp-match? #rx"^(?:boot[0-9]+/)?o[0-9]+$" (symbol->string id))))))

(define (app-to-view a)
  (render-view (app-items a)
               (app-cursor a)
               (app-scroll a)
               (app-width a)
               (app-height a)
               (app-file-path a)
               (length (app-items a))
               #:search-mode? (app-search-mode? a)
               #:search-query (app-search-query a)
               #:cursor-describable? (cursor-describable? a)))

(module+ main
  (define file-path (command-line #:program "fasl-viewer" #:args (file) file))

  (define pf (parse-file file-path))
  (define initial-app (make-app pf))

  (run initial-app
       #:alt-screen #t
       #:on-key app-on-key
       #:on-msg app-on-msg
       #:to-view app-to-view))
