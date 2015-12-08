#lang racket
(require "idris-interaction.rkt")
(require racket/gui)

(provide repl)

;;;; Graphical REPL as demo/test for interactions

(define idris-eventspace (current-eventspace))

(define (queue-idris-output proc)
  (parameterize ((current-eventspace idris-eventspace))
    (queue-callback proc #t)))

;; This code uses Racket Esq. in the Typed Racket testsuite for
;; inspiration, but departs in a few ways. Eventually it will also
;; support snips that represent the Idris notion of semantic
;; highlighting, and it will then need to depart massively!
(define idris-repl-text%
  (class text%
    (init-field eval-callback)
    (inherit insert last-position get-text get-style-list change-style set-position)

    (super-new)

    (define my-style-list (get-style-list))

    (define idris-prompt-style
      (send my-style-list new-named-style
            "Idris prompt" (send my-style-list basic-style)))

    (send idris-prompt-style set-delta
          (make-object style-delta% 'change-bold))

    (define idris-input-style
      (send my-style-list new-named-style
            "Idris REPL input" (send my-style-list basic-style)))

    (send idris-input-style set-delta
          (make-object style-delta% 'change-normal))

    (define idris-semantic-highlight-style
      (send my-style-list new-named-style
            "Idris semantic highlight" (send my-style-list basic-style)))
    (define idris-semantic-function-highlight-style
      (send my-style-list new-named-style
            "Idris semantic function highlight" idris-semantic-highlight-style))
    (define idris-semantic-type-highlight-style
      (send my-style-list new-named-style
            "Idris semantic type highlight" idris-semantic-highlight-style))
    (define idris-semantic-data-highlight-style
      (send my-style-list new-named-style
            "Idris semantic data highlight" idris-semantic-highlight-style))

    (send idris-semantic-function-highlight-style set-delta
          (let ((delta (make-object style-delta% 'change-nothing)))
            (send delta set-delta-foreground (make-object color% 0 128 0 1.0))
            delta))

    (send idris-semantic-type-highlight-style set-delta
          (let ((delta (make-object style-delta% 'change-nothing)))
            (send delta set-delta-foreground (make-object color% 0 0 128 1.0))
            delta))

    (send idris-semantic-data-highlight-style set-delta
          (let ((delta (make-object style-delta% 'change-nothing)))
            (send delta set-delta-foreground (make-object color% 200 0 0 1.0))
            delta))


    ;; Map an Idris decor keyword to a style
    (define (idris-get-decor-style decor)
      (match decor
        [':type     idris-semantic-type-highlight-style]
        [':function idris-semantic-function-highlight-style]
        [':data     idris-semantic-data-highlight-style]
        [other      #f]))

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
            (eval-callback
             (get-text input-beginning-position
                       (- (last-position) 1))))
          (super on-char c)))

    (define/public (insert-prompt)
      (queue-idris-output
       (λ ()
         (set! locked? #f)
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
           (let ([decor (assoc ':decor info)])
             (when decor
               (let ([style (idris-get-decor-style (cadr decor))])
                 (when style
                   (change-style style
                                 (+ base-pos start)
                                 (+ base-pos start len)
                                 #f)))))]
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

(define idris-repl-frame%
  (class frame%
    (field (my-idris-thread #f))

    (super-new [label "Idris"] [width 700] [height 1000])

    (define panel (new vertical-panel% [parent this]))
    (define start-idris
      (new button%
           [parent panel]
           [label "Start Idris"]
           [callback (lambda (x y)
                       (set! my-idris-thread (idris-thread))
                       (send start-idris enable #f)
                       (send output-editor insert-prompt))]))

    (define output-editor
      (new idris-repl-text%
           [eval-callback
            (lambda (cmd)
              (thread-send my-idris-thread
                           `(send (:interpret ,cmd)
                                  ,(lambda (res [highlights empty])
                                     (send output-editor output res highlights)
                                     (send output-editor output "\n")
                                     ;; this is the success cont, so we can insert
                                     ;; a prompt here
                                     (send output-editor insert-prompt))
                                  ,(lambda (res [highlights empty])
                                     void)
                                  ,(lambda (res [highlights empty])
                                     (send output-editor output res highlights)
                                     (send output-editor output "\n")
                                     ;; this is the error cont, so we
                                     ;; should also give a prompt
                                     (send output-editor insert-prompt)
                                     ))))]))
    (define output (new editor-canvas% [parent panel] [editor output-editor]))))


(define (repl)
  (define frame (new idris-repl-frame%))
  (send frame show #t))

(module+ main (repl))