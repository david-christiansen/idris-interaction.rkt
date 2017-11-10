#lang racket
(require "has-idris.rkt")
(require "idris-editor-commands.rkt")
(require "idris-tag.rkt")
(require "idris-tag-details.rkt")
(require "idris-repl-text.rkt")
(require "idris-highlighting-text.rkt")
(require "idris-token-editor.rkt")
(require "idris-error.rkt")
(require racket/gui)
(require framework)

(define (first-line str)
  (let ([lines (string-split str "\n")])
    (if (cons? lines)
        (car lines)
        str)))

(define idris-editor-frame%
  (class (has-idris-mixin frame%)
    (inherit start-my-idris idris-send
             get-idris-working-directory set-idris-working-directory
             set-label)

    (super-new [label "Idris editor"] [width 1000] [height 700])

    (define inhibit-ask-save? (make-parameter #f))

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
                       (queue-callback
                        (thunk
                         (send start-idris enable #f)
                         (send run enable #t)
                         (send repl-editor insert-prompt))))]))


    (define (highlight-code editor-file-name highlights)
      (for ([hl highlights])
        (match hl
          [(list (list-no-order (list ':filename filename)
                                (list ':start s-line s-col)
                                (list ':end e-line e-col))
                 (list-no-order (list ':decor decor) rest ...))
           ;; Filter out spurious highlights
           #:when (string-suffix? editor-file-name filename)
           (let* ([start-line-start-pos
                   (send code-editor line-start-position
                         (send code-editor idris-line->editor-line s-line))]
                  [start-pos (+ start-line-start-pos s-col -1)]
                  [end-line-start-pos
                   (send code-editor line-start-position
                         (send code-editor idris-line->editor-line e-line))]
                  [end-pos (+ end-line-start-pos e-col -1)])
             ;; filter more garbage
             (when (< start-pos end-pos)
               (send code-editor add-idris-highlight
                     start-pos end-pos
                     (idris-tag-from-protocol
                      (cons (list ':decor decor)
                            rest)))))]
          [other void])))


    (define (report-error an-error)
      (match-let ([(idris-error file
                                (app (lambda (l) (send code-editor idris-line->editor-line l)) start-line)
                                start-col
                                (app (lambda (l) (send code-editor idris-line->editor-line l)) end-line)
                                end-col
                                text
                                highlights)
                   an-error])
        (let* ([error-pos-string
                (if (and (= start-line end-line)
                         (= start-col end-col))
                    ;; point error
                    (format "~a:~a" (+ 1 start-line) start-col)
                    ;; span error
                    (format "~a:~a–~a:~a"
                            (+ 1 start-line) start-col
                            (+ 1 end-line) end-col))]
               [summary (first-line text)])
          (send error-list append error-pos-string an-error)
          (send error-list set-string
                (- (send error-list get-number) 1)
                (if (< (string-length summary) 200)
                    summary
                    (substring summary 0 199))
                1))))

    (define (run-in-idris)
      (if (not editor-file-name)
          (message-box "Can't run unsaved program"
                       "To send your program to Idris, please save it first."
                       this
                       '(ok caution))
          (begin
            (when (send code-editor is-modified?)
              (when (or (inhibit-ask-save?)
                        (equal?
                         (message-box "Unsaved code"
                                      "Idris will see the last saved version of your code.\n\nSave before running?"
                                      this
                                      '(yes-no))
                         'yes))
                (save-file)))
            (let-values ([(base name must-be-dir)
                          (split-path editor-file-name)])
              (unless (equal? base (get-idris-working-directory))
                (set-idris-working-directory base))
              ;; We need to delete the IBC name to make Idris retypecheck, which gets
              ;; highlighting for freshly opened files.
              (let ([ibc-name (path-replace-extension editor-file-name ".ibc")])
                (when (file-exists? ibc-name)
                  (delete-file ibc-name)))
              (idris-send `(:load-file ,(path->string name))
                          #:on-success
                          (lambda (msg [highlighting empty])
                            (send repl-editor insert-prompt))
                          #:on-output
                          (lambda (msg [highlighting empty])
                            (match msg
                              [(list ':set-prompt str _)
                               ;; Here we ignore Idris's requests for a prompt
                               ;; because they're too long
                               #;(send repl set-prompt str)
                               (void)]
                              [(list ':write-string str _)
                               (send repl-editor output str)]
                              [(list ':highlight-source hls)
                               (highlight-code (path->string name) hls)]
                              [(list ':warning
                                     (list filename
                                           (list start-line start-col)
                                           (list end-line end-col)
                                           text
                                           highlights)
                                     _)
                               (report-error
                                (idris-error filename
                                             start-line start-col
                                             end-line end-col
                                             text highlights))]
                              [other (displayln (format "Other: ~a" other))
                                     #;
                                     (message-box "Idris output"
                                                  (format "~a" other)
                                                  frame
                                                  '(ok caution))]))
                          #:on-error
                          display-output-details)))))

    (define run
      (new button%
           [parent toolbar]
           [label "Run"]
           [enabled #f]
           [callback (lambda args (run-in-idris))]))

    (define (tag-popup tag)
      (if (idris-tag? tag)
          (let ([the-menu
                 (new popup-menu%
                      [title (and (idris-tag? tag)
                                  (idris-tag-full-name tag))])])
            (new menu-item%
                 [label "Info"]
                 [parent the-menu]
                 [callback (lambda args (send info-widget set-tag tag))])
            the-menu)
          #f))

    (define horizontal
      (new panel:horizontal-dragable%
           [parent vertical]))
    (define code-editor
      (new (idris-token-editor-mixin idris-highlighting-text%) [tag-menu-callback tag-popup]))
    (define text-editor-canvas (new editor-canvas% [parent horizontal] [editor code-editor]))
    (add-idris-keys code-editor this (thunk (parameterize ([inhibit-ask-save? #t]) (run-in-idris)))
                    #:on-success (lambda (res [hl '()])
                                   (send repl-editor output "\n")
                                   (send repl-editor output res hl)
                                   (send repl-editor insert-prompt))
                    #:on-error (lambda (res [hl '()])
                                 (send repl-editor output "\n")
                                 (send repl-editor output res hl)
                                 (send repl-editor insert-prompt))
                    #:auto-load? #f)

    (define right-panel (new vertical-panel% [parent horizontal]))

    (define (feedback-holder-callback panel event)
      (when (eqv? (send event get-event-type) 'tab-panel)
        (send panel change-children
              (lambda (_)
                (match (send panel get-selection)
                  [0 (list repl-editor-canvas)]
                  [1 (list error-list)]
                  [2 (list output-details-canvas)])))))

    (define feedback-holder
      (new tab-panel%
           [parent right-panel]
           [choices '("REPL" "Errors" "Details")]
           [callback feedback-holder-callback]))

    (define (clear-output-details)
      (send* output-details
        (remove-highlighting)
        (do-edit-operation 'select-all)
        (do-edit-operation 'clear)))

    (define (display-output-details text [highlights empty])
      (clear-output-details)
      (send output-details insert text)
      (for ([hl highlights])
        (match hl
          [(list offset len (app idris-tag-from-protocol
                                 tag))
           (when tag
             (send output-details add-idris-highlight
                   offset (+ offset len)
                   tag))]))
      (switch-to-details-tab))

    (define error-list
      (new list-box%
           [label #f]
           [parent feedback-holder]
           [choices empty]
           [columns '("Location" "Summary")]
           [style (let ([style '(single column-headers)])
                    (cons 'deleted style))]
           [callback
            (lambda (list-box event)
              (when (equal? (send event get-event-type)
                            'list-box-dclick)
                (let ([selected-index (send list-box get-selections)])
                  (when (cons? selected-index)
                    (let ([error (send list-box get-data
                                       (car selected-index))])
                      (display-output-details (idris-error-text error)
                                              (idris-error-highlighting error))
                      (send code-editor set-position
                            (+ (send code-editor idris-line->editor-line
                                     (idris-error-start-line error))
                               (idris-error-start-column error))
                            (+ (send code-editor idris-line->editor-line
                                     (idris-error-end-line error))
                               (idris-error-end-column error))))))))]))

    (define output-details
      (new idris-highlighting-text%))
    (define output-details-canvas
      (new editor-canvas%
           [parent feedback-holder]
           [editor output-details]
           [style '(deleted)]))

    (define (switch-to-details-tab)
      (send* feedback-holder
        (set-selection 2)
        (change-children
         (const (list output-details-canvas)))))

    (define repl-editor
      (new idris-repl-text%
           [tag-menu-callback tag-popup]
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

    (define repl-editor-canvas (new editor-canvas% [parent feedback-holder] [editor repl-editor]))
    (define info-widget (new idris-tag-details-widget% [parent right-panel] [tag #f]))))

(define (editor)
  (parameterize ([application:current-app-name "Idris Editor"])
    (define frame (new idris-editor-frame%))
    (send frame show #t)))

(module+ main
  (editor))
