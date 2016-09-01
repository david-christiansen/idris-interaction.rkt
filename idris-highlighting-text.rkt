#lang racket

(require racket/gui)
(require data/interval-map)
(require "idris-tag.rkt")

(provide idris-highlighting-text% idris-highlighting-editor<%>)

(define idris-basic-style-delta
  (make-parameter
   (let ([δ (make-object style-delta%)])
     (send δ set-delta 'change-family 'modern))))


(define idris-highlighting-editor<%>
  (interface ()
    ;; Allow for hidden lines
    [idris-line->editor-line (->m exact-nonnegative-integer? exact-nonnegative-integer?)]
    [editor-line->idris-line (->m exact-nonnegative-integer? exact-nonnegative-integer?)]

    ;; Interpret a tag to get a style
    [get-idris-decor-style (->m idris-tag? (or/c #f (is-a?/c style<%>)))]

    ;; Style part of the editor
    [add-idris-highlight (->m exact-nonnegative-integer?
                              exact-nonnegative-integer?
                              idris-tag?
                              void?)]

    [remove-highlighting (->m void?)]

    [tag-at-position (->m exact-nonnegative-integer?
                          (or/c idris-tag? #f))]))

(define idris-highlighting-text%
  (class* text% (idris-highlighting-editor<%>)
    (init [line-spacing 1.0]
          [tab-stops null]
          [auto-wrap #f])
    (init-field [tag-menu-callback #f])

    (super-new [line-spacing line-spacing]
               [tab-stops tab-stops]
               [auto-wrap auto-wrap])

    (inherit change-style
             find-position
             get-active-canvas
             get-style-list
             last-position)

    (define/public (idris-line->editor-line line) (sub1 line))
    (define/public (editor-line->idris-line line) (add1 line))

    (define my-style-list (get-style-list))

    (define basic-style (send my-style-list basic-style))
    (let ([new-basic-style
           (send my-style-list find-or-create-style
                 basic-style
                 (idris-basic-style-delta))])
      (send my-style-list replace-named-style "Standard" basic-style))


    (define idris-semantic-function-highlight-style
      (let ((delta (make-object style-delta% 'change-nothing)))
        (send delta set-delta-foreground (make-object color% 31 122 122 1.0))
        delta))

    (define idris-semantic-type-highlight-style
      (let ((delta (make-object style-delta% 'change-nothing)))
        (send delta set-delta-foreground (make-object color% 0 0 174 1.0))
        delta))

    (define idris-semantic-data-highlight-style
      (let ((delta (make-object style-delta% 'change-nothing)))
        (send delta set-delta-foreground (make-object color% 200 0 0 1.0))
        delta))

    (define idris-semantic-bound-highlight-style
      (let ((delta (make-object style-delta% 'change-nothing)))
        (send delta set-delta-foreground (make-object color% 200 0 200 1.0))
        delta))

    (define idris-keyword-highlight-style
      (make-object style-delta% 'change-bold))

    (define idris-hole-highlight-style
      (make-object style-delta% 'change-italic))

    ;; Map an Idris decor keyword to a style
    (define/public (get-idris-decor-style tag)
      (match (idris-tag-decor tag)
        [':type     idris-semantic-type-highlight-style]
        [':function idris-semantic-function-highlight-style]
        [':data     idris-semantic-data-highlight-style]
        [':bound    idris-semantic-bound-highlight-style]
        [':keyword  idris-keyword-highlight-style]
        [':metavar  idris-hole-highlight-style]
        [other      #f]))

    ;;; Highlight a region
    (define highlights (make-interval-map))

    (define/public (add-idris-highlight start end tag)
      (when (<= end start)
        (error (format "Invalid range ~a--~a" start end)))
      (when tag
        (interval-map-set! highlights start end tag)
        (let ([style (get-idris-decor-style tag)])
          (when style
            (change-style style start end #f)))))

    (define/public (remove-highlighting)
      (change-style (let ([δ (make-object style-delta%)])
                      (send* δ
                        (set-delta 'change-normal-color)
                        (set-delta 'change-weight 'normal)
                        (set-delta 'change-style 'normal))
                      δ)
                    0
                    (last-position))
      (set! highlights (make-interval-map)))

    (define/augment (on-insert start len)
      (interval-map-expand! highlights start (+ start len)))
    (define/augment (on-delete start len)
      (interval-map-contract! highlights start (+ start len)))

    (define/public (tag-at-position position)
      (interval-map-ref highlights position #f))

    (define/override (on-default-event mouse-event)
      (if (equal? (send mouse-event get-event-type)
                  'right-down)
          (let* ([x (send mouse-event get-x)]
                 [y (send mouse-event get-y)]
                 [maybe-tag (tag-at-position (find-position x y))])
            (when (and tag-menu-callback maybe-tag)
              (let ([menu (tag-menu-callback maybe-tag)]
                    [canvas (get-active-canvas)])
                (when (and menu canvas)
                  (send canvas popup-menu menu x y)))))
          (super on-default-event mouse-event)))))
