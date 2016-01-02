#lang racket

(require racket/gui)
(require "idris-highlighting-text.rkt")
(require "idris-tag.rkt")
(provide idris-eventspace queue-idris-output idris-repl-text%)

(define idris-eventspace (current-eventspace))

(define (queue-idris-output proc)
  (parameterize ((current-eventspace idris-eventspace))
    (queue-callback proc #t)))


;; This code uses Racket Esq. in the Typed Racket testsuite for
;; inspiration, but departs in a few ways. Eventually it will also
;; support snips that represent the Idris notion of semantic
;; highlighting, and it will then need to depart massively!
(define idris-repl-text%
  (class idris-highlighting-text%
    (init-field eval-callback)
    (init [line-spacing 1.0]
          [tab-stops null]
          [auto-wrap #f]
          [tag-menu-callback #f])

    (inherit insert last-position get-text get-style-list change-style set-position
             get-idris-decor-style add-idris-highlight set-styles-sticky)

    (super-new [line-spacing line-spacing]
               [tab-stops tab-stops]
               [auto-wrap auto-wrap]
               [tag-menu-callback tag-menu-callback])

    #;(repl-set-style this "PragmataPro" 400)

    (define my-style-list (get-style-list))

    (define idris-input-style
      (let ([δ (make-object style-delta%)])
        (send* δ
          (set-delta 'change-normal-color)
          (set-delta 'change-weight 'normal)
          (set-delta 'change-style 'normal))
        δ)
      #;
      (send my-style-list find-or-create-style
            (send my-style-list find-named-style "Standard")
            (make-object style-delta%)))

    (define idris-prompt-style
      (let ([δ (make-object style-delta% 'change-bold)])
        (send δ set-delta-foreground (make-object color% 0 0 0 1.0))
        δ)
      #;
      (send my-style-list find-or-create-style
            (send my-style-list find-named-style "Standard")
            (make-object style-delta% 'change-bold)))

    (define previous-input-beginning-position 0)
    (define input-beginning-position 0)
    (define current-prompt "")
    (define locked? #t)

    (define/public (set-prompt new-prompt)
      (set! current-prompt new-prompt))

    (define/augment (can-insert? start len)
      (and (>= start input-beginning-position)
           (not locked?)))

    (define/override (on-char c)
      (if (and (eq? (send c get-key-code)
                    #\return)
               (not locked?))
          (begin
            (set-position (last-position))
            (super on-char c)
            (set! locked? #t)
            (set! previous-input-beginning-position input-beginning-position)
            (eval-callback
             (get-text input-beginning-position
                       (- (last-position) 1))))
          (super on-char c)))

    (define/public (insert-prompt)
      (queue-idris-output
       (λ ()
         (set! locked? #f)
         (set-position (last-position))
         (unless (or (= (last-position) 0)
                     (string=? (get-text (- (last-position) 1) (last-position))
                               "\n"))
           (insert "\n"))
         (change-style idris-prompt-style)
         (insert current-prompt)
         (insert ">")
         (change-style idris-input-style)
         (insert " ")
         (set! input-beginning-position (last-position)))))

    (define (highlight-from base-pos highlights)
      (for ([hl highlights])
        (match hl
          [(list start len info)
           (let ([tag (idris-tag-from-protocol info)])
             (when tag
               (add-idris-highlight (+ base-pos start)
                                    (+ base-pos start len)
                                    tag)))]
          [other void])))

    (define/public (highlight-repl-input highlights)
      (for ([hl highlights])
        (match hl
          [(list (list-no-order (list ':filename "(input)")
                                (list ':start 1 s-col)
                                (list ':end 1 e-col))
                 (list-no-order (list ':decor decor) rest ...))
           #:when (> e-col s-col)
           (highlight-from previous-input-beginning-position
                           (list (list (- s-col 1)
                                       (- e-col s-col)
                                       `((:decor ,decor)))))]
          [other void])))

    (define/public (output str [highlights empty])
      (queue-idris-output
       (lambda ()
         (let ((was-locked? locked?)
               (insertion-base (last-position)))
           (set! locked? #f)
           (insert str)
           (highlight-from insertion-base highlights)
           (set! locked? was-locked?)))))))
