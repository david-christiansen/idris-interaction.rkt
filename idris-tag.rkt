#lang racket

(provide (struct-out idris-tag)
         idris-tag-from-protocol)

(struct idris-tag (info full-name doc-overview decor type implicit?) #:transparent)

(define (idris-tag-from-protocol info)
  (define (perhaps elt)
    (if elt (cadr elt) #f))
  (let ([decor (perhaps (assoc ':decor info))]
        [full-name (perhaps (assoc ':name info))]
        [doc-overview (perhaps (assoc ':doc-overview info))]
        [type (perhaps (assoc ':type info))]
        [implicit? (perhaps (assoc ':implicit info))])
    (if decor
        (idris-tag info
                   full-name
                   doc-overview
                   decor
                   type
                   implicit?)
        #f)))
