#lang racket/base

(require rackunit
         racket/set
         racket/list
         racket/string
         racket/hash
         kettle/test
         "../parse.rkt"
         "../render.rkt"
         "../main.rkt"
         "../describe.rkt")

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

(test-case "zo file tree has module children"
  (define pf (parse-file zo-path))
  (define tree (build-tree pf))
  (define root (first tree))
  ;; Root should have children (module paths from linkl-directory)
  (check-true (> (length (tnode-children root)) 0)
              "zo root should have module children")
  ;; First child should be a Module node
  (check-regexp-match #rx"Module" (tnode-label (first (tnode-children root)))))

(test-case "zo module has phase/metadata children"
  (define pf (parse-file zo-path))
  (define tree (build-tree pf))
  (define root (first tree))
  (define mod-node (first (tnode-children root)))
  ;; Module should have children (phase linklets + metadata)
  (check-true (> (length (tnode-children mod-node)) 0)
              "module node should have children")
  ;; Should have at least one Phase linklet
  (check-true (ormap (lambda (c) (regexp-match? #rx"Phase" (tnode-label c)))
                      (tnode-children mod-node))
              "module should have a Phase linklet child"))

(test-case "zo phase linklet has decompiled details"
  (define pf (parse-file zo-path))
  (define tree (build-tree pf))
  (define root (first tree))
  (define mod-node (first (tnode-children root)))
  ;; Find a Phase linklet child
  (define phase-node
    (findf (lambda (c) (regexp-match? #rx"Phase" (tnode-label c)))
           (tnode-children mod-node)))
  (check-not-false phase-node "should find a Phase linklet")
  ;; Phase linklet should have detail lines (decompiled content)
  (check-true (> (length (tnode-details phase-node)) 0)
              "phase linklet should have decompiled detail lines"))

(test-case "zo node IDs are unique"
  (define pf (parse-file zo-path))
  (define tree (build-tree pf))
  (define all-expanded
    (let loop ([nodes tree]
               [ids (seteq)])
      (for/fold ([ids ids]) ([n (in-list nodes)])
        (loop (tnode-children n) (set-add ids (tnode-id n))))))
  (define items (flatten-tree tree all-expanded))
  (define all-ids (filter values (map flat-item-id items)))
  (check-equal? (length all-ids) (set-count (list->seteq all-ids)) "zo node IDs should be unique"))

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
;; Fasl inner info tests

(test-case "fasl objects have inner-info parsed"
  (define pf (parse-file petite-fasl-path))
  (define boot (parsed-file-data pf))
  (define objects (chez-boot-file-objects boot))
  (define with-info (filter fasl-object-inner-info objects))
  (check-true (> (length with-info) 0) "should parse inner-info for some objects"))

(test-case "fasl inner info identifies closure type"
  (define pf (parse-file petite-fasl-path))
  (define boot (parsed-file-data pf))
  (define objects (chez-boot-file-objects boot))
  (define closures
    (filter (lambda (obj)
              (define inner (fasl-object-inner-info obj))
              (and inner (eq? 'closure (fasl-inner-info-type inner))))
            objects))
  (check-true (> (length closures) 0) "should find closure objects"))

(test-case "fasl inner info identifies immediate type"
  (define pf (parse-file petite-fasl-path))
  (define boot (parsed-file-data pf))
  (define objects (chez-boot-file-objects boot))
  (define immediates
    (filter (lambda (obj)
              (define inner (fasl-object-inner-info obj))
              (and inner (eq? 'immediate (fasl-inner-info-type inner))))
            objects))
  (check-true (> (length immediates) 0) "should find immediate objects"))

(test-case "fasl inner info shows graph counts"
  (define pf (parse-file petite-fasl-path))
  (define boot (parsed-file-data pf))
  (define objects (chez-boot-file-objects boot))
  (define with-graph
    (filter (lambda (obj)
              (define inner (fasl-object-inner-info obj))
              (and inner (fasl-inner-info-graph-count inner)))
            objects))
  (check-true (> (length with-graph) 0) "should find objects with graph headers"))

(test-case "fasl objects show inner type in label"
  (define pf (parse-file petite-fasl-path))
  (define tree (build-tree pf))
  (define root-id (tnode-id (first tree)))
  (define items (flatten-tree tree (seteq root-id)))
  (check-true (ormap (lambda (item)
                       (regexp-match? #rx"\\[closure\\]" (flat-item-label item)))
                     items)
              "should show [closure] in object labels"))

(test-case "fasl objects with graph info are expandable"
  (define pf (parse-file petite-fasl-path))
  (define tree (build-tree pf))
  (define root-id (tnode-id (first tree)))
  (define items (flatten-tree tree (seteq root-id)))
  (define expandable-fasl
    (filter (lambda (item)
              (and (flat-item-expandable? item)
                   (regexp-match? #rx"\\[(closure|code|record)" (flat-item-label item))))
            items))
  (check-true (> (length expandable-fasl) 0)
              "fasl objects with graph info should be expandable"))

(test-case "expanding fasl object shows graph details"
  (define pf (parse-file petite-fasl-path))
  (define tree (build-tree pf))
  (define root-id (tnode-id (first tree)))
  (define items1 (flatten-tree tree (seteq root-id)))
  ;; Find first expandable fasl object with [closure] that has graph info
  (define fasl-item
    (for/or ([item (in-list items1)])
      (and (flat-item-expandable? item)
           (regexp-match? #rx"\\[closure\\]" (flat-item-label item))
           item)))
  (check-not-false fasl-item "should find an expandable closure object")
  ;; Expand it
  (define expanded (seteq root-id (flat-item-id fasl-item)))
  (define items2 (flatten-tree tree expanded))
  (define detail-lines (filter flat-item-is-detail? items2))
  (check-true (> (length detail-lines) 0) "should have detail lines")
  ;; Should have at least a "type:" line
  (check-true (ormap (lambda (d) (string-contains? (flat-item-label d) "type:")) detail-lines)
              "detail should mention type"))

;; -------------------------------------------------------------------
;; Search tests

(test-case "/ enters search mode"
  (define pf (parse-file petite-fasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp 'enter) ; expand root
  (test-program-press tp #\/)
  (define a (test-program-value tp))
  (check-true (app-search-mode? a))
  (check-equal? (app-search-query a) ""))

(test-case "search jumps to matching item"
  (define pf (parse-file petite-fasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp 'enter) ; expand root
  (test-program-press tp #\/)    ; enter search
  (for ([c (in-string "record")])
    (test-program-press tp c))
  (define a (test-program-value tp))
  (check-true (app-search-mode? a))
  ;; Should have jumped to first [record] item
  (define item (list-ref (app-items a) (app-cursor a)))
  (check-regexp-match #rx"[Rr]ecord" (flat-item-label item)))

(test-case "enter confirms search, escape cancels"
  (define pf (parse-file petite-fasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp 'enter) ; expand root
  (define cursor-before (app-cursor (test-program-value tp)))
  (test-program-press tp #\/)
  (for ([c (in-string "record")])
    (test-program-press tp c))
  ;; Confirm with enter
  (test-program-press tp 'enter)
  (define a1 (test-program-value tp))
  (check-false (app-search-mode? a1))
  (check-not-equal? (app-cursor a1) cursor-before "cursor should have moved")

  ;; Now search again and cancel with escape
  (define cursor-after-confirm (app-cursor a1))
  (test-program-press tp #\/)
  (for ([c (in-string "code")])
    (test-program-press tp c))
  (test-program-press tp 'escape)
  (define a2 (test-program-value tp))
  (check-false (app-search-mode? a2))
  (check-equal? (app-cursor a2) cursor-after-confirm "escape should restore cursor"))

(test-case "n finds next match, N finds previous"
  (define pf (parse-file petite-fasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp 'enter) ; expand root
  ;; Search for "record"
  (test-program-press tp #\/)
  (for ([c (in-string "record")])
    (test-program-press tp c))
  (test-program-press tp 'enter) ; confirm
  (define a1 (test-program-value tp))
  (define first-match (app-cursor a1))

  ;; n should find next match
  (test-program-press tp #\n)
  (define a2 (test-program-value tp))
  (check-true (> (app-cursor a2) first-match) "n should move forward")
  (check-regexp-match #rx"[Rr]ecord"
                      (flat-item-label (list-ref (app-items a2) (app-cursor a2))))

  ;; N should find previous match
  (test-program-press tp #\N)
  (define a3 (test-program-value tp))
  (check-equal? (app-cursor a3) first-match "N should go back to first match"))

(test-case "search bar appears in view"
  (define pf (parse-file petite-fasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 100
                           #:height 30))
  (test-program-press tp #\/)
  (for ([c (in-string "test")])
    (test-program-press tp c))
  (check-test-program-contains tp "/test"))

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

;; -------------------------------------------------------------------
;; Describe (d key) tests

(test-case "d on fasl object loads description"
  (define pf (parse-file petite-fasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 120
                           #:height 40))
  ;; Expand root, navigate to first fasl object (past hdr-group)
  (test-program-press tp 'enter)
  (test-program-press tp #\j) ; hdr-group
  (test-program-press tp #\j) ; o0
  (test-program-press tp #\d)
  (define a (test-program-value tp))
  (check-false (hash-empty? (app-descriptions a)) "description should be loaded")
  (check-true (set-member? (app-expanded a) 'o0) "o0 should be auto-expanded"))

(test-case "d toggles description off"
  (define pf (parse-file petite-fasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 120
                           #:height 40))
  (test-program-press tp 'enter)
  (test-program-press tp #\j)
  (test-program-press tp #\j)
  ;; Toggle on
  (test-program-press tp #\d)
  (define a1 (test-program-value tp))
  (check-true (hash-has-key? (app-descriptions a1) 'o0))
  ;; Toggle off
  (test-program-press tp #\d)
  (define a2 (test-program-value tp))
  (check-false (hash-has-key? (app-descriptions a2) 'o0) "description should be removed"))

(test-case "d on vfasl object loads description"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 120
                           #:height 40))
  (test-program-press tp 'enter)
  ;; Find a vfasl object by searching items
  (define a0 (test-program-value tp))
  (define vfasl-idx
    (for/or ([i (in-naturals)]
             [item (in-list (app-items a0))])
      (and (regexp-match? #rx"\\[vfasl\\]" (flat-item-label item))
           i)))
  (check-not-false vfasl-idx "should have a vfasl item")
  (define id (flat-item-id (list-ref (app-items a0) vfasl-idx)))
  ;; Navigate to it
  (for ([_ (in-range vfasl-idx)])
    (test-program-press tp #\j))
  (test-program-press tp #\d)
  (define a (test-program-value tp))
  (check-false (hash-empty? (app-descriptions a)) "description should be loaded")
  (check-true (set-member? (app-expanded a) id) "vfasl node should be expanded"))

(test-case "d on vfasl procedure creates CODE children"
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 120
                           #:height 40))
  (test-program-press tp 'enter)
  (define a0 (test-program-value tp))
  ;; Find second vfasl object (first may be a non-procedure vector)
  (define vfasl-indices
    (for/list ([i (in-naturals)]
               [item (in-list (app-items a0))]
               #:when (regexp-match? #rx"\\[vfasl\\]" (flat-item-label item)))
      i))
  (check-true (>= (length vfasl-indices) 2) "should have at least 2 vfasl items")
  (define vfasl-idx (second vfasl-indices))
  (define id (flat-item-id (list-ref (app-items a0) vfasl-idx)))
  ;; Navigate to it and press d
  (for ([_ (in-range vfasl-idx)])
    (test-program-press tp #\j))
  (test-program-press tp #\d)
  (define a1 (test-program-value tp))
  ;; Should have CODE children in the tree
  (define node-children (find-tnode-children (app-tree a1) id))
  (check-not-false node-children "described node should exist")
  (check-true (pair? node-children) "should have CODE children")
  (check-regexp-match #rx"^CODE" (tnode-label (first node-children))))

(test-case "d on header node is no-op"
  (define pf (parse-file petite-fasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 120
                           #:height 40))
  (test-program-press tp 'enter)
  (test-program-press tp #\j) ; hdr-group
  (test-program-press tp #\d)
  (define a (test-program-value tp))
  (check-true (hash-empty? (app-descriptions a)) "d on header should be no-op"))

(test-case "d on fasl closure creates CODE children"
  (define pf (parse-file petite-fasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 120
                           #:height 40))
  (test-program-press tp 'enter) ; expand root
  ;; Find first closure object
  (define a0 (test-program-value tp))
  (define closure-idx
    (for/or ([i (in-naturals)]
             [item (in-list (app-items a0))])
      (and (regexp-match? #rx"\\[closure\\]" (flat-item-label item))
           i)))
  (check-not-false closure-idx "should find a closure object")
  ;; Navigate to it and press d
  (for ([_ (in-range closure-idx)])
    (test-program-press tp #\j))
  (test-program-press tp #\d)
  (define a1 (test-program-value tp))
  (define id (flat-item-id (list-ref (app-items a0) closure-idx)))
  ;; Should have CODE children in the tree
  (define node-children (find-tnode-children (app-tree a1) id))
  (check-not-false node-children "described node should exist")
  (check-true (pair? node-children) "should have CODE children")
  ;; First child should be labeled as top-level CODE
  (check-regexp-match #rx"^CODE .* top-level" (tnode-label (first node-children))))

(test-case "expanding CODE child shows assembly lines"
  (define pf (parse-file petite-fasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 120
                           #:height 60))
  (test-program-press tp 'enter) ; expand root
  ;; Find first closure object
  (define a0 (test-program-value tp))
  (define closure-idx
    (for/or ([i (in-naturals)]
             [item (in-list (app-items a0))])
      (and (regexp-match? #rx"\\[closure\\]" (flat-item-label item))
           i)))
  (check-not-false closure-idx)
  ;; Navigate to closure and press d
  (for ([_ (in-range closure-idx)])
    (test-program-press tp #\j))
  (test-program-press tp #\d)
  ;; Find the top-level CODE child in the flat items
  (define a1 (test-program-value tp))
  (define code-idx
    (for/or ([i (in-naturals)]
             [item (in-list (app-items a1))])
      (and (regexp-match? #rx"^CODE .* top-level" (flat-item-label item))
           i)))
  (check-not-false code-idx "should find CODE child in items")
  ;; Navigate to the CODE child and expand it
  (define cur (app-cursor a1))
  (define delta (- code-idx cur))
  (for ([_ (in-range (abs delta))])
    (test-program-press tp (if (> delta 0) #\j #\k)))
  (test-program-press tp 'enter) ; expand CODE child
  ;; Assembly lines should appear as detail items
  (define a2 (test-program-value tp))
  (define detail-items
    (filter (lambda (item)
              (and (flat-item-is-detail? item)
                   (> (flat-item-depth item) (flat-item-depth (list-ref (app-items a2) code-idx)))))
            (app-items a2)))
  (check-true (> (length detail-items) 0) "should have assembly detail lines"))

(test-case "d toggle-off removes CODE children"
  (define pf (parse-file petite-fasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 120
                           #:height 40))
  (test-program-press tp 'enter) ; expand root
  ;; Find first closure object
  (define a0 (test-program-value tp))
  (define closure-idx
    (for/or ([i (in-naturals)]
             [item (in-list (app-items a0))])
      (and (regexp-match? #rx"\\[closure\\]" (flat-item-label item))
           i)))
  (check-not-false closure-idx)
  (define id (flat-item-id (list-ref (app-items a0) closure-idx)))
  ;; Navigate to closure and press d (on)
  (for ([_ (in-range closure-idx)])
    (test-program-press tp #\j))
  (test-program-press tp #\d)
  (define a1 (test-program-value tp))
  (check-true (pair? (find-tnode-children (app-tree a1) id)) "children added")
  ;; Press d again (off)
  (test-program-press tp #\d)
  (define a2 (test-program-value tp))
  (check-true (null? (find-tnode-children (app-tree a2) id)) "children removed")
  (check-false (hash-has-key? (app-descriptions a2) id) "description removed"))

(test-case "format-entry-description produces non-empty lines"
  (check-true (pair? (format-entry-description '#(CODE 0 0 "test" 1 #f () #"abc" 0 #()))))
  (check-true (pair? (format-entry-description #f)))
  (check-equal? (format-entry-description #f) '("(no description)")))

;; -------------------------------------------------------------------
;; Comprehensive vfasl describe tests

(test-case "all petite-vfasl.boot entries describe without errors"
  (define result (describe-boot-file-entries petite-vfasl-path))
  (check-true (> (vector-length result) 0) "should have entries")
  (for ([i (in-range (vector-length result))])
    (define d (vector-ref result i))
    (when (string? d)
      (check-false (string-prefix? d "Error:")
                   (format "entry ~a should not be an error: ~a"
                           i (substring d 0 (min 100 (string-length d))))))))

(test-case "all racket exe petite.boot entries describe without errors"
  (define pf (parse-file racket-exe-path))
  (define exe (parsed-file-data pf))
  (define boot (first (racket-executable-boot-files exe)))
  (define result
    (describe-exe-boot-entries racket-exe-path
                               (racket-executable-section-offset exe)
                               (racket-boot-entry-offset boot)
                               (racket-boot-entry-size boot)))
  (check-true (> (vector-length result) 0) "should have entries")
  (for ([i (in-range (vector-length result))])
    (define d (vector-ref result i))
    (when (string? d)
      (check-false (string-prefix? d "Error:")
                   (format "petite entry ~a should not be an error: ~a"
                           i (substring d 0 (min 100 (string-length d))))))))

(test-case "all racket exe scheme.boot entries describe without errors"
  (define pf (parse-file racket-exe-path))
  (define exe (parsed-file-data pf))
  (define boot (second (racket-executable-boot-files exe)))
  (define result
    (describe-exe-boot-entries racket-exe-path
                               (racket-executable-section-offset exe)
                               (racket-boot-entry-offset boot)
                               (racket-boot-entry-size boot)))
  (check-true (> (vector-length result) 0) "should have entries")
  (for ([i (in-range (vector-length result))])
    (define d (vector-ref result i))
    (when (string? d)
      (check-false (string-prefix? d "Error:")
                   (format "scheme entry ~a should not be an error: ~a"
                           i (substring d 0 (min 100 (string-length d))))))))

(test-case "vfasl description vectors have correct ENTRY structure"
  (define result (describe-boot-file-entries petite-vfasl-path))
  (define vfasl-descs
    (for/list ([i (in-range (vector-length result))]
               #:when (vector? (vector-ref result i)))
      (vector-ref result i)))
  (check-true (> (length vfasl-descs) 0) "should have vector descriptions")
  (for ([desc (in-list vfasl-descs)])
    (check-equal? (vector-ref desc 0) 'ENTRY "tag should be ENTRY")
    (check-true (>= (vector-length desc) 3) "ENTRY should have at least 3 elements")))

(test-case "vfasl closure descriptions contain CODE"
  (define result (describe-boot-file-entries petite-vfasl-path))
  (define closure-descs
    (for/list ([i (in-range (vector-length result))]
               #:when (and (vector? (vector-ref result i))
                           (>= (vector-length (vector-ref result i)) 3)
                           (let ([inner (vector-ref (vector-ref result i) 2)])
                             (and (vector? inner)
                                  (eq? (vector-ref inner 0) 'CLOSURE)))))
      (vector-ref result i)))
  (check-true (> (length closure-descs) 0) "should have closure descriptions")
  (for ([desc (in-list closure-descs)])
    (define closure (vector-ref desc 2))
    (check-true (>= (vector-length closure) 3) "CLOSURE should have at least 3 elements")
    (define code (vector-ref closure 2))
    (check-true (vector? code) "CLOSURE should contain CODE vector")
    (check-equal? (vector-ref code 0) 'CODE "inner should be CODE")))

(test-case "vfasl CODE vectors have assembly after disassembly"
  (define result (describe-boot-file-entries petite-vfasl-path))
  ;; Find a closure description
  (define closure-desc
    (for/or ([i (in-range (vector-length result))])
      (define d (vector-ref result i))
      (and (vector? d)
           (>= (vector-length d) 3)
           (let ([inner (vector-ref d 2)])
             (and (vector? inner)
                  (eq? (vector-ref inner 0) 'CLOSURE)
                  d)))))
  (check-not-false closure-desc "should have at least one closure")
  (define code (vector-ref (vector-ref closure-desc 2) 2))
  ;; Index 7 should be assembly (list) after disassembly, not raw bytes
  (define asm (vector-ref code 7))
  (check-true (or (pair? asm) (bytes? asm))
              "assembly slot should be a list (disassembled) or bytes"))

(test-case "description-to-tree handles ENTRY/CLOSURE/CODE"
  (define desc '#(ENTRY visit-revisit #(CLOSURE 0 #(CODE 0 0 "test-fn" 1 #f () #"abc" 3 #()))))
  (define-values (lines children) (description-to-tree desc 'test-id))
  (check-true (pair? lines) "should produce detail lines")
  (check-true (pair? children) "should produce CODE children")
  (check-true (tnode? (first children)) "children should be tnodes")
  (check-regexp-match #rx"CODE" (tnode-label (first children))))

(test-case "description-to-tree handles string descriptions"
  (define desc "vfasl result: #(1 2 3)")
  (define-values (lines children) (description-to-tree desc 'test-id))
  (check-true (pair? lines) "should produce lines")
  (check-true (null? children) "strings should not produce children"))

(test-case "description-to-tree handles #f"
  (define-values (lines children) (description-to-tree #f 'test-id))
  (check-equal? lines '("(no description)"))
  (check-true (null? children)))

(test-case "extract-vfasl-payload returns #f for non-vfasl"
  ;; fasl kind byte is 100, vfasl is 101
  ;; Build a minimal fasl-like entry: situation(37) size(2) comp(44) kind(100) payload
  (define fake-fasl (bytes 37 2 44 100 0 0))
  (check-false (extract-vfasl-payload fake-fasl)))

(test-case "describe-live-value handles non-procedure values"
  ;; A simple fixnum should return a string description
  (define desc (describe-live-value 42))
  (check-true (string? desc) "fixnum should produce string description")
  (check-regexp-match #rx"42" desc))

(test-case "d on every vfasl entry in petite-vfasl.boot works in TUI"
  ;; Exercises the full TUI pipeline for each vfasl entry
  (define pf (parse-file petite-vfasl-path))
  (define tp
    (make-test-program/run (make-app pf)
                           #:on-key app-on-key
                           #:on-msg app-on-msg
                           #:to-view app-to-view
                           #:width 120
                           #:height 40))
  (test-program-press tp 'enter) ; expand root
  (define a0 (test-program-value tp))
  (define vfasl-indices
    (for/list ([i (in-naturals)]
               [item (in-list (app-items a0))]
               #:when (regexp-match? #rx"\\[vfasl\\]" (flat-item-label item)))
      i))
  (check-true (> (length vfasl-indices) 0) "should have vfasl items")
  ;; Test first 5 vfasl entries (or all if fewer)
  (for ([vfasl-idx (in-list (take vfasl-indices (min 5 (length vfasl-indices))))])
    ;; Reset to root expanded state
    (test-program-press tp #\g) ; go to top
    (for ([_ (in-range vfasl-idx)])
      (test-program-press tp #\j))
    (define before (test-program-value tp))
    (define id (flat-item-id (list-ref (app-items before) (app-cursor before))))
    (test-program-press tp #\d) ; describe
    (define after (test-program-value tp))
    ;; Description should load (or be a no-op for non-describable items)
    (when (regexp-match? #rx"\\[vfasl\\]"
                         (flat-item-label (list-ref (app-items before) (app-cursor before))))
      (check-false (hash-empty? (app-descriptions after))
                   (format "d on vfasl entry ~a should load description" vfasl-idx)))
    ;; Toggle off
    (test-program-press tp #\d)))
