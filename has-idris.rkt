#lang racket
(require "idris-interaction.rkt")
(provide has-idris<%> has-idris-mixin)

(define has-idris<%>
  (interface ()
    [start-my-idris (->m void?)]
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
                         error-handler)))))


