#lang racket
(require racket/gui)
(require "idris-tag.rkt")
(provide idris-tag-details-widget%)


(define idris-tag-details-widget%
  (class vertical-panel%
    (init parent)
    (init-field tag)

    (super-new [parent parent]
               [stretchable-height #f]
               [alignment '(left top)])

    (define (name-type-string)
      (let ([perhaps (lambda (str) (or str ""))]) 
        (if (idris-tag? tag)
            (string-append
             (perhaps (idris-tag-full-name tag))
             (perhaps (and (idris-tag-full-name tag)
                           (idris-tag-type tag)
                           " : "))
             (perhaps (idris-tag-type tag))
             (if (and (idris-tag-full-name tag)
                      (idris-tag-type tag))
                 "\n"
                 ""))
            "")))

    (define (doc-string)
      (and (idris-tag? tag)
           (idris-tag-doc-overview tag)))

    (define name-type-view
      (new message%
           [parent this]
           [label (if (> (string-length (name-type-string)) 0)
                      name-type-string
                      "")]))
    (define doc-view
      (new message%
           [parent this]
           [label (if (doc-string) (doc-string) "")]))

    (define/public (set-tag new-tag)
      (set! tag new-tag)
      (let ([name-type (name-type-string)]
            [doc (doc-string)])
        (send name-type-view set-label name-type)
        (send doc-view set-label (if (doc-string) (doc-string) ""))))))
