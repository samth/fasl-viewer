#lang racket/base

(require rackunit
         racket/set
         racket/list
         racket/string
         kettle/test
         "../parse.rkt"
         "../render.rkt"
         "../main.rkt")

;; -------------------------------------------------------------------
;; Test data: parse real files for realistic testing

(define petite-vfasl-path "/home/samth/sw/ChezScheme/experiments/petite-vfasl.boot")
(define petite-fasl-path "/home/samth/sw/ChezScheme/ta6le/boot/ta6le/petite.boot")
(define racket-exe-path "/home/samth/sw/plt/racket/bin/racket")
(define zo-path "/home/samth/sw/plt/racket/collects/racket/compiled/main_rkt.zo")

;; -------------------------------------------------------------------
;; Tree building tests

(test-case "build-tree produces a single root for chez boot file"
  (define pf (parse-file petite-vfasl-path))
  (define tree (build-tree pf))
  (check-equal? (length tree) 1)
  (check-true (tnode-expandable? (first tree))))

(test-case "build-tree produces a single root for racket executable"
  (define pf (parse-file racket-exe-path))
  (define tree (build-tree pf))
  (check-equal? (length tree) 1)
  (check-true (tnode-expandable? (first tree))))

(test-case "build-tree produces a single root for zo file"
  (define pf (parse-file zo-path))
  (define tree (build-tree pf))
  (check-equal? (length tree) 1)
  (check-true (tnode-expandable? (first tree))))

;; -------------------------------------------------------------------
;; Unique node IDs

(test-case "node IDs are unique within a chez boot file"
  (define pf (parse-file petite-vfasl-path))
  (define tree (build-tree pf))
  (define all-expanded
    (let loop ([nodes tree]
               [ids (seteq)])
      (for/fold ([ids ids]) ([n (in-list nodes)])
        (loop (tnode-children n) (set-add ids (tnode-id n))))))
  (define items (flatten-tree tree all-expanded))
  (define all-ids (filter values (map flat-item-id items)))
  (check-equal? (length all-ids) (set-count (list->seteq all-ids)) "all node IDs should be unique"))

(test-case "node IDs are unique across embedded boot files in racket executable"
  (define pf (parse-file racket-exe-path))
  (define tree (build-tree pf))
  (define all-expanded
    (let loop ([nodes tree]
               [ids (seteq)])
      (for/fold ([ids ids]) ([n (in-list nodes)])
        (loop (tnode-children n) (set-add ids (tnode-id n))))))
  (define items (flatten-tree tree all-expanded))
  (define all-ids (filter values (map flat-item-id items)))
  (check-equal? (length all-ids)
                (set-count (list->seteq all-ids))
                "all node IDs should be unique across 3 boot files"))

(test-case "boot file prefixes are distinct"
  (define pf (parse-file racket-exe-path))
  (define tree (build-tree pf))
  (define all-expanded
    (let loop ([nodes tree]
               [ids (seteq)])
      (for/fold ([ids ids]) ([n (in-list nodes)])
        (loop (tnode-children n) (set-add ids (tnode-id n))))))
  (define ids all-expanded)
  ;; Each boot file has its own prefixed node IDs
  (check-true (set-member? ids 'boot0))
  (check-true (set-member? ids 'boot1))
  (check-true (set-member? ids 'boot2))
  (check-true (set-member? ids 'boot0/hdr-group))
  (check-true (set-member? ids 'boot1/hdr-group))
  (check-true (set-member? ids 'boot2/hdr-group)))

;; -------------------------------------------------------------------
;; Flatten tree tests

(test-case "collapsed tree has one item"
  (define pf (parse-file petite-vfasl-path))
  (define tree (build-tree pf))
  (define items (flatten-tree tree (seteq)))
  (check-equal? (length items) 1))

(test-case "expanding root shows children"
  (define pf (parse-file petite-vfasl-path))
  (define tree (build-tree pf))
  (define root-id (tnode-id (first tree)))
  (define items (flatten-tree tree (seteq root-id)))
  (check-true (> (length items) 1))
  ;; First item should be root, expanded
  (check-true (flat-item-expanded? (first items)))
  ;; Children should be at depth 1
  (check-true (andmap (lambda (i) (>= (flat-item-depth i) 1)) (rest items))))

(test-case "expanding a vfasl object shows detail lines"
  (define pf (parse-file petite-vfasl-path))
  (define tree (build-tree pf))
  (define root-id (tnode-id (first tree)))
  (define items1 (flatten-tree tree (seteq root-id)))
  ;; Find first expandable vfasl object (match [vfasl] tag, not filename)
  (define vfasl-item
    (for/or ([item (in-list items1)])
      (and (flat-item-expandable? item)
           (regexp-match? #rx"\\[vfasl\\]" (flat-item-label item))
           item)))
  (check-not-false vfasl-item)
  ;; Expand it
  (define expanded (seteq root-id (flat-item-id vfasl-item)))
  (define items2 (flatten-tree tree expanded))
  ;; Should have detail lines
  (define detail-lines (filter flat-item-is-detail? items2))
  (check-true (> (length detail-lines) 0))
  ;; Detail lines should mention vspace names
  (check-true (ormap (lambda (d) (string-contains? (flat-item-label d) "symbol")) detail-lines))
  (check-true (ormap (lambda (d) (string-contains? (flat-item-label d) "code")) detail-lines)))

;; -------------------------------------------------------------------
;; Cursor position stability during expand/collapse

(test-case "toggle-expand keeps cursor on same item"
  (define pf (parse-file petite-vfasl-path))
  (define a0 (make-app pf))
  ;; Expand root
  (define a1
    (test-program-value (let ([tp (make-test-program/run a0
                                                         #:on-key app-on-key
                                                         #:on-msg app-on-msg
                                                         #:to-view app-to-view
                                                         #:width 100
                                                         #:height 30)])
                          (test-program-press tp 'enter) ; expand root
                          tp)))
  (check-equal? (app-cursor a1) 0)
  (check-true (set-member? (app-expanded a1) (tnode-id (first (app-tree a1)))))
  ;; Item at cursor should be the root
  (check-equal? (flat-item-id (list-ref (app-items a1) 0)) (tnode-id (first (app-tree a1)))))

(test-case "expand then collapse keeps cursor on same item"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  ;; Expand root
  (test-program-press tp 'enter)
  (define a1 (test-program-value tp))
  (define items-after-expand (length (app-items a1)))
  (check-true (> items-after-expand 1))
  ;; Collapse root
  (test-program-press tp 'enter)
  (define a2 (test-program-value tp))
  (check-equal? (app-cursor a2) 0)
  (check-equal? (length (app-items a2)) 1))

(test-case "expanding item mid-list keeps cursor on same item"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  ;; Expand root, move down to a vfasl object
  (test-program-press tp 'enter)
  (for ([_ (in-range 6)])
    (test-program-press tp #\j))
  (define a-before (test-program-value tp))
  (define cursor-before (app-cursor a-before))
  (define item-before (list-ref (app-items a-before) cursor-before))
  (define scroll-before (app-scroll a-before))
  ;; Expand it
  (test-program-press tp 'enter)
  (define a-after (test-program-value tp))
  (define cursor-after (app-cursor a-after))
  (define item-after (list-ref (app-items a-after) cursor-after))
  (define scroll-after (app-scroll a-after))
  ;; Cursor should be on same item
  (check-equal? (flat-item-id item-after)
                (flat-item-id item-before)
                "cursor should stay on the same item after expand")
  ;; Scroll should not change
  (check-equal? scroll-after scroll-before "scroll should not change when expanding at cursor")
  ;; Screen position should be the same
  (check-equal? (- cursor-after scroll-after)
                (- cursor-before scroll-before)
                "screen row should be the same after expand"))

(test-case "collapsing item mid-list keeps cursor on same item"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  ;; Expand root, move to a vfasl object, expand it
  (test-program-press tp 'enter)
  (for ([_ (in-range 6)])
    (test-program-press tp #\j))
  (test-program-press tp 'enter)
  (define a-expanded (test-program-value tp))
  (define cursor-exp (app-cursor a-expanded))
  (define item-exp (list-ref (app-items a-expanded) cursor-exp))
  (define scroll-exp (app-scroll a-expanded))
  ;; Collapse it
  (test-program-press tp 'enter)
  (define a-collapsed (test-program-value tp))
  (define cursor-col (app-cursor a-collapsed))
  (define item-col (list-ref (app-items a-collapsed) cursor-col))
  (define scroll-col (app-scroll a-collapsed))
  ;; Same item, same position
  (check-equal? (flat-item-id item-col) (flat-item-id item-exp))
  (check-equal? scroll-col scroll-exp)
  (check-equal? (- cursor-col scroll-col) (- cursor-exp scroll-exp)))

;; -------------------------------------------------------------------
;; Independent expansion across boot files

(test-case "expanding object in one boot file does not affect others"
  (define pf (parse-file racket-exe-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 40))
  ;; Expand root
  (test-program-press tp 'enter)
  ;; Expand petite.boot (first child, now directly contains chez tree children)
  (test-program-press tp #\j)
  (test-program-press tp 'enter)
  ;; Move to first vfasl object (past hdr-group + 3 fasl objects = 4 items down)
  (for ([_ (in-range 5)])
    (test-program-press tp #\j))
  (test-program-press tp 'enter)
  (define a1 (test-program-value tp))
  ;; Check that a boot0/ object is expanded
  (define expanded (app-expanded a1))
  (define petite-obj-expanded?
    (for/or ([id (in-set expanded)])
      (and (regexp-match? #rx"^boot0/o" (symbol->string id)) id)))
  (check-not-false petite-obj-expanded? "a petite.boot object should be expanded")
  ;; The same-indexed object in scheme.boot should NOT be expanded
  (check-false (set-member? expanded
                            (string->symbol (regexp-replace #rx"^boot0/"
                                                            (symbol->string petite-obj-expanded?)
                                                            "boot1/")))
               "same-indexed object in scheme.boot should NOT be expanded"))

;; -------------------------------------------------------------------
;; Navigation tests

(test-case "j moves cursor down"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp 'enter) ; expand root
  (test-program-press tp #\j)
  (check-equal? (app-cursor (test-program-value tp)) 1))

(test-case "k moves cursor up"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp 'enter) ; expand root
  (test-program-press tp #\j)
  (test-program-press tp #\j)
  (test-program-press tp #\k)
  (check-equal? (app-cursor (test-program-value tp)) 1))

(test-case "k at top stays at 0"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp #\k)
  (check-equal? (app-cursor (test-program-value tp)) 0))

(test-case "g goes to top"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp 'enter) ; expand root
  (for ([_ (in-range 10)])
    (test-program-press tp #\j))
  (test-program-press tp #\g)
  (check-equal? (app-cursor (test-program-value tp)) 0))

(test-case "G goes to bottom"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp 'enter) ; expand root
  (test-program-press tp #\G)
  (define a (test-program-value tp))
  (check-equal? (app-cursor a) (sub1 (length (app-items a)))))

(test-case "l expands and h collapses"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  ;; l should expand
  (test-program-press tp #\l)
  (define a1 (test-program-value tp))
  (check-true (set-member? (app-expanded a1) (tnode-id (first (app-tree a1)))))
  ;; h should collapse
  (test-program-press tp #\h)
  (define a2 (test-program-value tp))
  (check-false (set-member? (app-expanded a2) (tnode-id (first (app-tree a2))))))

(test-case "h on non-expanded item moves to parent"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp 'enter) ; expand root
  (test-program-press tp #\j) ; move to child
  (test-program-press tp #\j) ; move to next child
  (define cursor-before (app-cursor (test-program-value tp)))
  (check-true (> cursor-before 0))
  ;; h on a non-expandable item should move to parent (root at 0)
  (test-program-press tp #\h)
  (check-equal? (app-cursor (test-program-value tp)) 0))

(test-case "q quits"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp #\q)
  (check-test-program-done tp))

;; -------------------------------------------------------------------
;; View rendering tests

(test-case "view contains file name"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (check-test-program-contains tp "petite-vfasl.boot"))

(test-case "view shows expand arrow"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  ;; Collapsed: right-pointing arrow
  (check-test-program-contains tp "▸")
  ;; Expand
  (test-program-press tp 'enter)
  ;; Expanded: down-pointing arrow
  (check-test-program-contains tp "▾"))

(test-case "view shows vfasl details when expanded"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  ;; Expand root, move to first vfasl object (index 5), expand it
  (test-program-press tp 'enter)
  (for ([_ (in-range 5)])
    (test-program-press tp #\j))
  (test-program-press tp 'enter)
  (define view-str (test-program-view-string tp))
  (check-regexp-match #rx"symbol" view-str)
  (check-regexp-match #rx"code" view-str)
  (check-regexp-match #rx"data:" view-str))

(test-case "view shows footer with position"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (check-test-program-contains tp "1/1"))

(test-case "window resize updates dimensions"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 80
                           #:height 24))
  (test-program-resize tp 120 40)
  (define a (test-program-value tp))
  (check-equal? (app-width a) 120)
  (check-equal? (app-height a) 40))
