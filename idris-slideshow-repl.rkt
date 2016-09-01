#lang racket

(require "idris-repl-text.rkt")
(require "idris-highlighting-text.rkt")
(require "idris-tag.rkt")
(require "idris-token-editor.rkt")
(require "has-idris.rkt")

(require racket/gui)
(require framework)
(require slideshow)


(provide idris-slideshow-repl idris-slideshow-editor)

(define (repl-get-style-delta face size-in-pixels)
  (let ([δ (make-object style-delta%)])
    (send δ set-delta 'change-family 'modern)
    (send δ set-face face)
    (send δ set-delta 'change-size-in-pixels #t)
    (send δ set-delta 'change-size size-in-pixels)
    δ))

(define (repl-set-style editor face size-in-pixels)
  (let* ([style-list (send editor get-style-list)]
         [basic-style (send style-list basic-style)]
         [δ (repl-get-style-delta face size-in-pixels)])
    (let ([new-basic-style (send style-list find-or-create-style basic-style δ)])
      (send style-list replace-named-style "Standard" new-basic-style))))

(define (temp-idris-file contents)
  (let* ([file-name (make-temporary-file "Slide~a.idr")]
         [port (open-output-file file-name #:mode 'text #:exists 'truncate/replace)])
    (write-string contents port)
    (close-output-port port)
    file-name))

(define (idris-repl-in-frame face size-in-screen-pixels preamble)
  (lambda (frame)
    (define idris-handle (new idris-handle%))
    (define (repl-callback cmd)
      (send idris-handle idris-send `(:interpret ,cmd)
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
              (send output-editor insert-prompt))))
    (define output-editor
      (new idris-repl-text% [eval-callback repl-callback]))
    (repl-set-style output-editor face size-in-screen-pixels)
    (define output (new editor-canvas% [parent frame] [editor output-editor]))


    (send idris-handle start-my-idris)
    (send idris-handle idris-send
          '(:interpret ":consolewidth 50"))
    (send output-editor set-prompt "λΠ")
    (if preamble
        (let ([file (temp-idris-file preamble)])
          (displayln (format "Saving to ~a" file))
          (let-values ([(base name must-be-dir)
                        (split-path file)])
            (send idris-handle set-idris-working-directory base)
            (send idris-handle idris-send
                  `(:load-file ,(path->string file))
                  #:on-success
                  (thunk*
                   (send output-editor insert-prompt)
                   (send idris-handle idris-send
                         '(:interpret ":consolewidth 50")))
                  #:on-output
                  (lambda (msg [highlighting empty])
                    (match msg
                      [(list ':set-prompt str _)
                       ;; Here we ignore Idris's requests for a prompt
                       ;; because they're too long
                       #;(send repl set-prompt str)
                       (void)]
                      [(list ':write-string str _)
                       (void) #;(send repl output str)
                       ]
                      [(list ':warning
                             (list filename
                                   (list start-line start-col)
                                   (list end-line end-col)
                                   text
                                   highlights)
                             _)
                       (displayln (string-append "Error from REPL preamble: " text))
                       ]
                      [other (void)]))
                  #:on-error
                  (lambda (msg [highlighting empty])
                    (send output-editor output msg)))))
        (send output-editor insert-prompt))

    ;; Return a cleanup proc that stops Idris again -- FIXME use a
    ;; global Idris instance and restart as necessary
    (lambda ()
      (send idris-handle quit-my-idris)
      (displayln "closing REPL, Idris stopped"))))


(struct idris-error
  (filename
   start-line start-column
   end-line end-column
   text
   highlighting)
  #:transparent)


(define (first-line str)
  (let ([lines (string-split str "\n")])
    (if (cons? lines)
        (car lines)
        str)))

(define (line-count str)
  (for/sum ([ch (in-string str)])
    (if (char=? #\newline ch) 1 0)))

(define (idris-editor-in-frame code
                               preamble
                               repl?
                               face
                               size-in-screen-pixels
                               auto-load?)
  (define preamble-lines (+ 1 (line-count preamble)))
  (define (idris-line->editor-line line)
    (- line preamble-lines 1))
  (define (editor-line->idris-line line)
    (+ line preamble-lines 1))

  (lambda (frame)
    (define my-idris (new idris-handle%))
    (define outer-container
      (new vertical-panel% [parent frame]))
    (define toolbar
      (new horizontal-panel%
           [parent outer-container]
           [stretchable-height #f]))
    (define inner-container
      (new panel:vertical-dragable%
           [parent outer-container]))
    (define code-editor
      (new (idris-token-editor-mixin idris-highlighting-text%)))

    (define (repl-callback cmd)
      (send my-idris idris-send `(:interpret ,cmd)
            #:on-success
            (lambda (res [highlights empty])
              (send repl output res highlights)
              (send repl output "\n")
              ;; this is the success cont, so we can insert
              ;; a prompt here
              (send repl insert-prompt))
            #:on-output
            (lambda (res [highlights empty])
              (match res
                [(list ':highlight-source highlighting)
                 (send repl highlight-repl-input
                       highlighting)]
                [other void]))
            #:on-error
            (lambda (res [highlights empty])
              (send repl output res highlights)
              (send repl output "\n")
              ;; this is the error cont, so we
              ;; should also give a prompt
              (send repl insert-prompt))))

    (define repl
      (new idris-repl-text% [eval-callback repl-callback]))
    (define text-editor-canvas
      (new editor-canvas% [parent inner-container] [editor code-editor]))

    (define (detail-holder-callback panel event)
      (when (equal? (send event get-event-type) 'tab-panel)
        (match (+ (send panel get-selection)
                  (if repl? 0 1))
          [0 (send panel change-children
                   (lambda (_) (list repl-canvas)))]
          [1 (send panel change-children
                   (lambda (_) (list error-list)))]
          [2 (send panel change-children
                   (lambda (_) (list output-details-canvas)))])))

    (define detail-holder
      (new tab-panel%
           [parent inner-container]
           [choices (let ([base-choices
                           '("Errors" "Details")])
                      (if repl?
                          (cons "REPL" base-choices)
                          base-choices))]
           [callback detail-holder-callback]))
    (define repl-canvas
      (if repl?
          (new editor-canvas%
               [parent detail-holder]
               [editor repl])
          #f))

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
           [parent detail-holder]
           [choices empty]
           [columns '("Location" "Summary")]
           [style (let ([style '(single column-headers)])
                    (if repl? (cons 'deleted style) style))]
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
                            (+ (idris-line->editor-line
                                (idris-error-start-line error))
                               (idris-error-start-column error))
                            (+ (idris-line->editor-line
                                (idris-error-end-line error))
                               (idris-error-end-column error))))))))]))

    (define output-details
      (new idris-highlighting-text%))
    (define output-details-canvas
      (new editor-canvas%
           [parent detail-holder]
           [editor output-details]
           [style '(deleted)]))

    (define (switch-to-details-tab)
      (send* detail-holder
        (set-selection (if repl? 2 1))
        (change-children
         (const (list output-details-canvas)))))

    (define (idris-info-command cmd)
      (send my-idris idris-send cmd
            #:on-success display-output-details
            #:on-error display-output-details))

    (define (editor-point)
      (let ([pos-start (box 0)]
            [pos-end (box 0)])
        (send code-editor get-position pos-start pos-end)
        (if (= (unbox pos-start) (unbox pos-end))
            (unbox pos-start)
            #f)))

    (define (idris-name-command cmd-name)
      (thunk*
       (let ([point (editor-point)])
         (when point
           (let ([tag (send code-editor tag-at-position point)])
             (when tag
               (idris-info-command `(,cmd-name ,(idris-tag-full-name tag)))))))))

    (define (idris-editing-command cmd-name insert-or-replace . extra)
      (thunk*
       (let ([point (editor-point)])
         (when point
           (let* ([editor-line (send code-editor position-line point)]
                  [line-num (editor-line->idris-line editor-line)]
                  [name-at-point (send code-editor idris-token-at-position point)])
             (when name-at-point
               (send my-idris idris-send
                     `(,cmd-name ,line-num ,name-at-point ,@extra)
                     #:on-success
                     (λ (code [hightlights empty])
                       (match insert-or-replace
                         ['insert (send code-editor insert
                                        (string-append "\n" code)
                                        (send code-editor line-end-position editor-line))]
                         ['replace (let ([line-beginning (if (= editor-line 0)
                                                             0
                                                             (send code-editor line-end-position (- editor-line 1)))]
                                         [line-end (send code-editor line-end-position editor-line)])
                                     (send code-editor insert
                                           (string-append "\n" code) line-beginning line-end))]
                         ['replace-hole (let ([hole-beginning (send code-editor find-string "?" 'backward point 'eof #f)]
                                              [hole-end (let loop ([looking-at point])
                                                          (if (let ([ch (send code-editor get-character looking-at)])
                                                                (or (char-alphabetic? ch)
                                                                    (char-numeric? ch)
                                                                    (char=? ch #\_)))
                                                              (loop (+ looking-at 1))
                                                              looking-at))])
                                          (send code-editor insert code hole-beginning hole-end))])
                       (when auto-load? (load-editor)))
                     #:on-error (λ args (displayln (format "oops ~a" args))))))))))

    (define (add-idris-keys editor)
      (let ([keymap (send editor get-keymap)])
        (send* keymap
          ;; info commands
          (add-function "Get docs" (idris-name-command ':docs-for))
          (map-function "c:c;c:d" "Get docs")
          (add-function "Get type" (idris-name-command ':type-of))
          (map-function "c:c;c:t" "Get type")
          ;; editing commands
          (add-function "Load into Idris" (thunk* (load-editor)))
          (map-function "c:c;c:l" "Load into Idris")
          (add-function "Start definition" (idris-editing-command ':add-clause 'insert))
          (map-function "c:c;c:s" "Start definition")
          (add-function "Case split" (idris-editing-command ':case-split 'replace))
          (map-function "c:c;c:c" "Case split")
          (add-function "Proof search" (idris-editing-command ':proof-search 'replace-hole empty))
          (map-function "c:c;c:a" "Proof search"))
        (send editor set-keymap keymap)))

    (define (highlight-code editor-file-name highlights)
      (for ([hl highlights])
        (match hl
          [(list (list-no-order (list ':filename filename)
                                (list ':start s-line s-col)
                                (list ':end e-line e-col))
                 (list-no-order (list ':decor decor) rest ...))
           ;; Filter out spurious highlights
           #:when (and (string-suffix? editor-file-name filename)
                       (> s-line preamble-lines))
           (let* ([start-line-start-pos
                   (send code-editor line-start-position
                         (idris-line->editor-line s-line))]
                  [start-pos (+ start-line-start-pos s-col -1)]
                  [end-line-start-pos
                   (send code-editor line-start-position
                         (idris-line->editor-line e-line))]
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
                                (app idris-line->editor-line start-line)
                                start-col
                                (app idris-line->editor-line end-line)
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
          (send error-list set-string (- (send error-list get-number) 1) summary 1))))

    (define (load-editor)
      (send code-editor remove-highlighting)
      (send error-list set empty empty)
      (send* output-details
        (do-edit-operation 'select-all)
        (do-edit-operation 'clear))
      (let ([file (temp-idris-file
                   (string-append preamble
                                  "\n"
                                  (send code-editor get-text)))])
        (displayln (format "Saving to ~a" file))
        (let-values ([(base name must-be-dir)
                      (split-path file)])
          (send my-idris set-idris-working-directory base)
          (send my-idris idris-send
                `(:load-file ,(path->string file))
                #:on-success
                (thunk*
                 (send repl insert-prompt)
                 (send my-idris idris-send
                       '(:interpret ":consolewidth 50")))
                #:on-output
                (lambda (msg [highlighting empty])
                  (match msg
                    [(list ':set-prompt str _)
                     ;; Here we ignore Idris's requests for a prompt
                     ;; because they're too long
                     #;(send repl set-prompt str)
                     (void)]
                    [(list ':write-string str _)
                     (send repl output str)]
                    [(list ':highlight-source hls)
                     (highlight-code (path->string file) hls)]
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
                (lambda (msg [highlighting empty])
                  (send repl output msg))))))
    (define load-button (new button%
                             [parent toolbar]
                             [label "Load"]
                             [callback (thunk* (load-editor))]))
    (send my-idris start-my-idris)
    (send my-idris idris-send '(:interpret ":consolewidth 50"))
    (repl-set-style code-editor face size-in-screen-pixels)
    (repl-set-style output-details face size-in-screen-pixels)
    (add-idris-keys code-editor)
    (when repl?
      (send repl set-prompt "λΠ")
      (repl-set-style repl face size-in-screen-pixels))
    (send code-editor set-styles-sticky #f)
    (send code-editor insert code)
    (when auto-load?
      (load-editor))
    (lambda ()
      (send my-idris quit-my-idris)
      (displayln "closing editor, killed idris"))))


(define (idris-slideshow-repl #:width [width 800]
                              #:height [height 600]
                              #:face [face #f]
                              #:size [size-in-slideshow-px 40]
                              #:fallback-pict [fallback (text "Idris interaction")]
                              #:preamble [preamble #f])
  (let* ([pict-area (dc (lambda (x y z) '()) width height)]
         [contents (cc-superimpose pict-area fallback)])
    (let-values ([(w h) (get-display-size)])
      (interactive contents
                   (idris-repl-in-frame (or face "Courier")
                                        (floor (* size-in-slideshow-px
                                                  (/ h 768)))
                                        preamble)))))

(define (idris-slideshow-editor code
                                #:width [width 800]
                                #:height [height 600]
                                #:auto-load? [auto-load? #f]
                                #:preamble [preamble ""]
                                #:repl? [repl? #f]
                                #:face [face #f]
                                #:size [size-in-slideshow-px 40]
                                #:fallback-pict [fallback (text "Idris editor")])
  (let* ([pict-area (dc (lambda (x y z) '()) width height)]
         [contents (cc-superimpose pict-area fallback)])
    (let-values ([(w h) (get-display-size)])
      (interactive contents
                   (idris-editor-in-frame
                    (if (list? code)
                        (string-join code "\n") code)
                    (if (list? preamble) (apply string-append preamble) preamble)
                    repl?
                    (or face "Courier")
                    (floor (* size-in-slideshow-px
                              (/ h 768)))
                    auto-load?)))))
