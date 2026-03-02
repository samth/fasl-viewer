#lang racket/base

;; main.rkt — Interactive TUI viewer for Chez Scheme fasl/vfasl files.

(require racket/cmdline
         racket/match
         racket/set
         racket/string
         kettle
         kettle/run
         "parse.rkt"
         "render.rkt")

(provide make-app
         app-on-key
         app-on-msg
         app-to-view
         (struct-out app))

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
         search-origin) ; cursor position before search started
  #:transparent)

(define (make-app pf)
  (define tree (build-tree pf))
  (define expanded (seteq)) ; start collapsed
  (define items (flatten-tree tree expanded))
  (app tree items 0 expanded 80 24 0 (parsed-file-path pf) #f "" 0))

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

(define (app-to-view a)
  (render-view (app-items a)
               (app-cursor a)
               (app-scroll a)
               (app-width a)
               (app-height a)
               (app-file-path a)
               (length (app-items a))
               #:search-mode? (app-search-mode? a)
               #:search-query (app-search-query a)))

(module+ main
  (define file-path (command-line #:program "fasl-viewer" #:args (file) file))

  (define pf (parse-file file-path))
  (define initial-app (make-app pf))

  (run initial-app
       #:alt-screen #t
       #:on-key app-on-key
       #:on-msg app-on-msg
       #:to-view app-to-view))
