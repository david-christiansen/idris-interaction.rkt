#lang racket
(require "has-idris.rkt")
(require "idris-repl-text.rkt")
(require racket/gui)

(define idris-editor-frame%
  (class (has-idris-mixin frame%)
    (inherit start-my-idris idris-send
             get-idris-working-directory set-idris-working-directory
             set-label)
    (super-new [label "Idris editor"] [width 1000] [height 700])

    (define editor-file-name #f)

    (define idris-file-name-filters '(("Idris" "*.idr") ("Any" "*.*")))

    (define (save-file-as)
      (let ([new-file-name
             (put-file "Where to save?"
                       this
                       (and editor-file-name
                            (let-values (((base name must-be-dir)
                                          (split-path editor-file-name)))
                              (and (path? base) base)))
                       #f
                       "idr"
                       empty
                       idris-file-name-filters)])
        (when new-file-name
          (send code-editor save-file new-file-name 'text)
          (set! editor-file-name new-file-name)
          (set-label (path->string new-file-name)))))

    (define (save-file)
      (if editor-file-name
          (send code-editor save-file editor-file-name 'text)
          (save-file-as)))

    (define (open-file)
      (let ([new-file-name
             (get-file "Open Idris code"
                       this
                       (and editor-file-name
                            (let-values (((base name must-be-dir)
                                          (split-path editor-file-name)))
                              (and (path? base) base)))
                       #f
                       "idr"
                       empty
                       idris-file-name-filters)])
        (send code-editor load-file new-file-name 'text)
        (set! editor-file-name new-file-name)
        (set-label (path->string new-file-name))))

    (define menu-bar (new menu-bar% [parent this]))
    (define file-menu (new menu% [parent menu-bar] [label "File"]))
    (define open (new menu-item%
                      [parent file-menu]
                      [label "Open..."]
                      [callback (lambda args (open-file))]))
    (define save (new menu-item%
                      [parent file-menu]
                      [label "Save"]
                      [callback (lambda args (save-file))]))
    (define save-as (new menu-item%
                         [parent file-menu]
                         [label "Save as..."]
                         [callback (lambda args (save-file-as))]))

    (define vertical (new vertical-panel% [parent this]))
    (define toolbar (new horizontal-panel% [parent vertical] [stretchable-height #f]))
    (define start-idris
      (new button%
           [parent toolbar]
           [label "Start Idris"]
           [callback (lambda (x y)
                       (start-my-idris)
                       (send start-idris enable #f)
                       (send run enable #t)
                       (send repl-editor insert-prompt))]))

    (define (run-in-idris)
      (if (not editor-file-name)
          (message-box "Can't run unsaved program"
                       "To send your program to Idris, please save it first."
                       this
                       '(ok caution))
          (begin
            (when (send code-editor is-modified?)
              (when (equal? (message-box "Unsaved code"
                                         "Idris will see the last saved version of your code.\n\nSave before running?"
                                         this
                                         '(yes-no))
                            'yes)
                (save-file)))
            (let-values ([(base name must-be-dir)
                          (split-path editor-file-name)])
              (unless (equal? base (get-idris-working-directory))
                (set-idris-working-directory base))
              (idris-send `(:load-file ,(path->string name))
                          #:on-success
                          (lambda (msg [highlighting empty])
                            (send repl-editor insert-prompt))
                          #:on-output
                          (lambda (msg [highlighting empty])
                            (match msg
                              [(list ':set-prompt str _)
                               (send repl-editor set-prompt str)]
                              [(list ':write-string str _)
                               (send repl-editor output str)]
                              [other void]))
                          #:on-error
                          (lambda (msg [highlighting empty])
                            (message-box "Idris error"
                                         (format "~a" msg)
                                         this
                                         '(ok caution))))))))

    (define run
      (new button%
           [parent toolbar]
           [label "Run"]
           [enabled #f]
           [callback (lambda args (run-in-idris))]))

    (define horizontal (new horizontal-panel% [parent vertical]))
    (define code-editor (new text%))
    (define repl-editor (new idris-repl-text%
                             [eval-callback
                              (lambda (cmd)
                                (idris-send `(:interpret ,cmd)
                                            #:on-success
                                            (lambda (res [highlights empty])
                                              (send repl-editor output res highlights)
                                              (send repl-editor output "\n")
                                              ;; this is the success cont, so we can insert
                                              ;; a prompt here
                                              (send repl-editor insert-prompt))
                                            #:on-output
                                            (lambda (res [highlights empty])
                                              (match res
                                                [(list ':highlight-source highlighting)
                                                 (send repl-editor highlight-repl-input
                                                       highlighting)]
                                                [other void]))
                                            #:on-error
                                            (lambda (res [highlights empty])
                                              (send repl-editor output res highlights)
                                              (send repl-editor output "\n")
                                              ;; this is the error cont, so we
                                              ;; should also give a prompt
                                              (send repl-editor insert-prompt))))]))

    (define text-editor-canvas (new editor-canvas% [parent horizontal] [editor code-editor]))
    (define repl-editor-canvas (new editor-canvas% [parent horizontal] [editor repl-editor]))))

(define (editor)
  (define frame (new idris-editor-frame%))
  (send frame show #t))
