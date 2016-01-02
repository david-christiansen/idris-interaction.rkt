#lang racket

(require racket/gui)

(provide idris-token-editor<%> idris-token-editor-mixin)

(define idris-token-editor<%>
  (interface ()
    [idris-token-at-position (->m exact-nonnegative-integer?
                                  (or/c string? #f))]))

(define (idris-name-character? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (char=? ch #\_)))

(define idris-operator-characters (string->list ":!#$%&*+./<=>?@\\^|-~"))

(define (idris-operator-character? ch)
  (member ch idris-operator-characters))

(define-syntax-rule (while go? body1 body ...)
  (let loop ()
    (if go?
        (begin body1 body ... (loop))
        void)))

(define idris-token-editor-mixin
  (mixin ((class->interface text%)) (idris-token-editor<%>)
    (init [line-spacing 1.0]
          [tab-stops null]
          [auto-wrap #f])
    (super-new [line-spacing line-spacing]
               [tab-stops tab-stops]
               [auto-wrap auto-wrap])

    (inherit get-character get-text)

    (define/public (idris-token-at-position pos)
      (let* ([start-ch (get-character pos)]
             [start-pos pos]
             [end-pos pos]
             [find (lambda (ok?)
                     (while (and (>= start-pos 0)
                                 (ok? (get-character start-pos)))
                       (set! start-pos (- start-pos 1)))
                     (while (ok? (get-character end-pos))
                       (set! end-pos (+ end-pos 1)))
                     (get-text (+ 1 start-pos) end-pos #t))])
        (cond [(idris-name-character? start-ch)
               (find idris-name-character?)]
              [(idris-operator-character? start-ch)
               (find idris-operator-character?)]
              [else #f])))))
