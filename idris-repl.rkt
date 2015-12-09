#lang racket
(require "has-idris.rkt")
(require "idris-repl-text.rkt")
(require racket/gui)

(provide repl idris-repl-text%)

;;;; Graphical REPL as demo/test for interactions



(define idris-repl-frame%
  (class (has-idris-mixin frame%)

    (inherit start-my-idris idris-send)

    (super-new [label "Idris"] [width 700] [height 1000])

    (define panel (new vertical-panel% [parent this]))
    (define start-idris
      (new button%
           [parent panel]
           [label "Start Idris"]
           [callback (lambda (x y)
                       (start-my-idris)
                       (send start-idris enable #f)
                       (send output-editor insert-prompt))]))

    (define output-editor
      (new idris-repl-text%
           [eval-callback
            (lambda (cmd)
              (idris-send `(:interpret ,cmd)
                          #:on-success
                          (lambda (res [highlights empty])
                            (send output-editor output res highlights)
                            (send output-editor output "\n")
                            ;; this is the success cont, so we can insert
                            ;; a prompt here
                            (send output-editor insert-prompt))
                          #:on-output
                          (lambda (res [highlights empty])
                            (match res
                              [(list ':highlight-source highlighting)
                               (send output-editor highlight-repl-input
                                     highlighting)]
                              [other void]))
                          #:on-error
                          (lambda (res [highlights empty])
                            (send output-editor output res highlights)
                            (send output-editor output "\n")
                            ;; this is the error cont, so we
                            ;; should also give a prompt
                            (send output-editor insert-prompt))))]))
    (define output (new editor-canvas% [parent panel] [editor output-editor]))))


(define (repl)
  (define frame (new idris-repl-frame%))
  (send frame show #t))

(module+ main (repl))
