#lang racket/base
(require types)
(require binary-tree)
(provide (all-defined-out))
(module+ test
  (require rackunit))


(define search-tree-type (make-type '(search-tree)))

(define make-search-tree
  (let ((pack (typed-value-packer search-tree-type)))
    (lambda (= < tree)
      (pack (cons (cons = <) tree)))))

;; root of all search trees is the empty-search-tree
(define make-empty-search-tree
  (lambda (= <)
    (make-search-tree = < empty-tree)))

(define search-tree?
  (typed-value-predicate search-tree-type))

(define search-tree-data
  (typed-value-unpacker search-tree-type))

(define search-tree-<
  (lambda (search-tree)
    (cdr (car (search-tree-data search-tree)))))

(define search-tree-=
  (lambda (search-tree)
    (car (car (search-tree-data search-tree)))))

(define search-tree-tree
  (lambda (search-tree)
    (cdr (search-tree-data search-tree))))

(define search-tree-member?
  (lambda (element search-tree)
    (let ((= (search-tree-= search-tree))
          (< (search-tree-< search-tree)))
      (letrec
          ((member? (lambda (tree)
                      (cond ((empty-tree? tree) #f)
                            ((= (node-label tree) element) #t)
                            ((< element (node-label tree)) (member? (node-left-branch tree)))
                            (else (member? (node-right-branch tree)))))))
        (member? (search-tree-tree search-tree))))))

(define search-tree-insert
  (lambda (element search-tree)
    (let ((= (search-tree-= search-tree))
          (< (search-tree-< search-tree)))
      (letrec
          ((insert (lambda (tree)
                     (cond ((empty-tree? tree) (make-node element empty-tree empty-tree))
                           ((= element (node-label tree)) tree)
                           ((< element (node-label tree)) (make-node (node-label tree)
                                                                     (insert (node-left-branch tree))
                                                                     (node-right-branch tree)))
                           (else (make-node (node-label tree)
                                            (node-left-branch tree)
                                            (insert (node-right-branch tree))))))))
        (make-search-tree = < (insert (search-tree-tree search-tree)))))))

(define search-tree-leaf?
  (lambda (search-tree)
    (let ((tree (search-tree tree)))
      (and (empty-tree? (node-left-branch tree))
           (empty-tree? (node-right-branch tree))))))

(define search-tree-minimum
  (lambda (search-tree)
    (let ((= (search-tree-= search-tree))
          (< (search-tree-< search-tree)))
      (letrec
          ((minimum (lambda (tree)
                      (cond ((empty-tree? tree) empty-tree)
                            (else
                             (let ((left (empty-tree? (node-left-branch tree))))
                               (if (empty-tree? left)
                                   tree
                                   (minimum left))))))))
        (minimum (search-tree-tree search-tree))))))

(define search-tree-maximum
  (lambda (search-tree)
    (let ((= (search-tree-= search-tree))
          (< (search-tree-< search-tree)))
      (letrec
          ((maximum (lambda (tree)
                      (cond ((empty-tree? tree) empty-tree)
                            (else
                             (let ((right (empty-tree? (node-right-branch tree))))
                               (if (empty-tree? right)
                                   tree
                                   (maximumg right))))))))
        (maximum (search-tree-tree search-tree))))))

#|
;; nice explanation of deletion of binary search tree
;; https://www.youtube.com/watch?v=gcULXE7ViZw
(define search-tree-delete
  (lambda (element search-tree)
    (let ((= (search-tree-= search-tree))
          (< (search-tree-< search-tree)))
      (letrec
          ((delete (lambda (tree)
                     (cond ((empty-tree? tree) empty-tree)
                           ((= (node-label tree) element)
                            (let* ((left (node-left-branch tree))
                                   (right (node-right-branch tree)))
                              (cond ((and (empty-tree? left)
                                          (empty-tree? right)) empty-tree) ;; node
                                    ((empty-tree? left)
                                     (node-right-branch tree))
                                    ((empty-tree? right)
                                     (node-right-branch left))
                                    (else
                                     (make-node (node-label (search-tree-minimum 
|#                                
                          
          

                                 
(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
