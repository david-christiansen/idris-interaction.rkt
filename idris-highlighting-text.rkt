#lang racket

(require racket/gui)
(require data/interval-map)
(require "idris-tag.rkt")

(provide idris-highlighting-text%)



(define idris-highlighting-editor<%>
  (interface ()
    ;; Interpret a tag to get a style
    [get-idris-decor-style (->m idris-tag? (or/c #f (is-a?/c style<%>)))]

    ;; Style part of the editor
    [add-idris-highlight (->m exact-nonnegative-integer?
                              exact-nonnegative-integer?
                              idris-tag?
                              void?)]))

(define idris-highlighting-text%
  (class* text% (idris-highlighting-editor<%>)
    (init [line-spacing 1.0]
          [tab-stops null]
          [auto-wrap #f])
    (init-field [tag-menu-callback #f])

    (super-new [line-spacing line-spacing]
               [tab-stops tab-stops]
               [auto-wrap auto-wrap])

    (inherit get-active-canvas get-style-list change-style find-position)


    (define my-style-list (get-style-list))

    (define basic-style (send my-style-list basic-style))
    (send basic-style set-delta
          (make-object style-delta% 'change-family 'modern))
    (send my-style-list replace-named-style "Standard" basic-style)

    (define idris-semantic-highlight-style
      (send my-style-list new-named-style
            "Idris semantic highlight" basic-style))
    (define idris-semantic-function-highlight-style
      (send my-style-list new-named-style
            "Idris semantic function highlight" idris-semantic-highlight-style))
    (define idris-semantic-type-highlight-style
      (send my-style-list new-named-style
            "Idris semantic type highlight" idris-semantic-highlight-style))
    (define idris-semantic-data-highlight-style
      (send my-style-list new-named-style
            "Idris semantic data highlight" idris-semantic-highlight-style))
    (define idris-semantic-bound-highlight-style
      (send my-style-list new-named-style
            "Idris semantic bound variable highlight" idris-semantic-highlight-style))
    (define idris-keyword-highlight-style
      (send my-style-list new-named-style
            "Idris keyword" idris-semantic-highlight-style))

    (send idris-semantic-function-highlight-style set-delta
          (let ((delta (make-object style-delta% 'change-nothing)))
            (send delta set-delta-foreground (make-object color% 0 128 0 1.0))
            delta))

    (send idris-semantic-type-highlight-style set-delta
          (let ((delta (make-object style-delta% 'change-nothing)))
            (send delta set-delta-foreground (make-object color% 0 0 174 1.0))
            delta))

    (send idris-semantic-data-highlight-style set-delta
          (let ((delta (make-object style-delta% 'change-nothing)))
            (send delta set-delta-foreground (make-object color% 200 0 0 1.0))
            delta))

    (send idris-semantic-bound-highlight-style set-delta
          (let ((delta (make-object style-delta% 'change-nothing)))
            (send delta set-delta-foreground (make-object color% 200 0 200 1.0))
            delta))

    (send idris-keyword-highlight-style set-delta
          (make-object style-delta% 'change-bold))

    ;; Map an Idris decor keyword to a style
    (define/public (get-idris-decor-style tag)
      (match (idris-tag-decor tag)
        [':type     idris-semantic-type-highlight-style]
        [':function idris-semantic-function-highlight-style]
        [':data     idris-semantic-data-highlight-style]
        [':bound    idris-semantic-bound-highlight-style]
        [':keyword  idris-keyword-highlight-style]
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

    (define/augment (on-insert start len)
      (interval-map-expand! highlights start (+ start len)))
    (define/augment (on-delete start len)
      (interval-map-contract! highlights start (+ start len)))

    (define/override (on-default-event mouse-event)
      (if (equal? (send mouse-event get-event-type)
                  'right-down)
          (let* ([x (send mouse-event get-x)]
                 [y (send mouse-event get-y)]
                 [click-position (find-position x y)]
                 [maybe-tag (interval-map-ref highlights click-position #f)])
            (when (and tag-menu-callback maybe-tag)
              (let ([menu (tag-menu-callback maybe-tag)]
                    [canvas (get-active-canvas)])
                (when (and menu canvas)
                  (send canvas popup-menu menu x y)))))
          (super on-default-event mouse-event)))))
