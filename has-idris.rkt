#lang racket
(require "idris-interaction.rkt")
(provide has-idris<%> idris-handle% has-idris-mixin)

(define has-idris<%>
  (interface ()
    [start-my-idris (->m void?)]
    [get-idris-working-directory (->m (or/c #f path?))]
    [set-idris-working-directory (->m path? void?)]
    [idris-send (->*m (any/c)
                      ;; TODO: more specific contract here
                      (#:on-success (->* (any/c) (any/c) void?)
                       #:on-output (->* (any/c) (any/c) void?)
                       #:on-error (->* (any/c) (any/c) void?))
                      void?)]))

(define has-idris-mixin
  (mixin () (has-idris<%>)
    (super-new)
    (define my-idris-thread #f)

    (define/public (start-my-idris)
      (unless my-idris-thread
        (set! my-idris-thread (idris-thread))))


    (define/public (idris-send ide-message
                               #:on-success [success-handler void]
                               #:on-output [progress-handler void]
                               #:on-error [error-handler void])
      (thread-send my-idris-thread
                   (list 'send
                         ide-message
                         success-handler
                         progress-handler
                         error-handler)))

    ;;; Maintain this state here, because Idris doesn't provide a good
    ;;; way to ask. TODO: fix this in Idris - it's silly to have this
    ;;; var both here and in idris-mode
    (define idris-working-directory #f)
    (define/public (get-idris-working-directory) idris-working-directory)
    (define/public (set-idris-working-directory new-wd)
      (idris-send `(:interpret ,(string-append ":cd "
                                               (path->string new-wd)))
                  #:on-success
                  (lambda args
                    (set! idris-working-directory new-wd))))))

(define idris-handle%
  (has-idris-mixin object%))

