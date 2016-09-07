#lang racket
(require racket/gui)
(require "idris-tag.rkt" "has-idris.rkt" "idris-highlighting-text.rkt" "idris-token-editor.rkt")

(provide add-idris-keys)

(define/contract (editor-point editor)
  (-> (is-a?/c editor<%>) (or/c exact-nonnegative-integer? #f))
  (let ([pos-start (box 0)]
        [pos-end (box 0)])
    (send editor get-position pos-start pos-end)
    (if (= (unbox pos-start) (unbox pos-end))
        (unbox pos-start)
        #f)))

(define/contract (idris-info-command idris-connection cmd #:on-success succeed #:on-error fail)
  (-> (is-a?/c has-idris<%>)
      any/c
      #:on-success (->* (string?)
                        ((listof (list/c exact-nonnegative-integer? exact-nonnegative-integer? any/c)))
                        any/c)
      #:on-error (->* (string?)
                      ((listof (list/c exact-nonnegative-integer? exact-nonnegative-integer? any/c)))
                      any/c)
      void?)
  (send idris-connection idris-send cmd
        #:on-success succeed
        #:on-error fail))

(define/contract (idris-name-command editor idris-connection cmd-name #:on-success succeed #:on-error fail)
  (-> (is-a?/c idris-highlighting-editor<%>)
      (is-a?/c has-idris<%>)
      symbol?
      #:on-success (->* (string?)
                        ((listof (list/c exact-nonnegative-integer? exact-nonnegative-integer? any/c)))
                        any/c)
      #:on-error (->* (string?)
                      ((listof (list/c exact-nonnegative-integer? exact-nonnegative-integer? any/c)))
                      any/c)
      (-> any/c (is-a?/c event%) any/c))
  (thunk*
   (let ([point (editor-point editor)])
     (when point
       (let ([tag (send editor tag-at-position point)])
         (when tag
           (idris-info-command idris-connection `(,cmd-name ,(idris-tag-full-name tag))
                               #:on-success succeed
                               #:on-error fail)))))))

(define/contract (idris-editing-command editor command-hook idris-connection cmd-name insert-or-replace . extra)
  (-> (and/c (is-a?/c idris-highlighting-editor<%>)
             (is-a?/c idris-token-editor<%>))
      (listof (-> void?))
      (is-a?/c has-idris<%>)
      symbol?
      (or/c 'insert 'replace 'replace-hole)
      any/c ...
      (-> any/c (is-a?/c event%) any/c))
  (thunk*
   (let ([point (editor-point editor)])
     (when point
       (let* ([editor-line (send editor position-line point)]
              [line-num (send editor editor-line->idris-line editor-line)]
              [name-at-point (send editor idris-token-at-position point)])
         (when name-at-point
           (displayln `(,cmd-name ,line-num ,name-at-point ,@extra))
           (send idris-connection idris-send
                 `(,cmd-name ,line-num ,name-at-point ,@extra)
                 #:on-success
                 (λ (code [hightlights empty])
                   (match insert-or-replace
                     ['insert (send editor insert
                                    (string-append "\n" code)
                                    (send editor line-end-position editor-line))]
                     ['replace (let ([line-beginning (if (= editor-line 0)
                                                         0
                                                         (send editor line-end-position (- editor-line 1)))]
                                     [line-end (send editor line-end-position editor-line)])
                                 (send editor insert
                                       (string-append "\n" code) line-beginning line-end))]
                     ['replace-hole (let ([hole-beginning (send editor find-string "?" 'backward point 'eof #f)]
                                          [hole-end (let loop ([looking-at point])
                                                      (if (let ([ch (send editor get-character looking-at)])
                                                            (or (char-alphabetic? ch)
                                                                (char-numeric? ch)
                                                                (char=? ch #\_)))
                                                          (loop (+ looking-at 1))
                                                          looking-at))])
                                      (send editor insert code hole-beginning hole-end))])
                   (for ([todo command-hook]) (todo)))
                 #:on-error (λ args (displayln (format "oops ~a" args))))))))))

(define/contract (add-idris-editor-menu editor connection
                                        #:on-success succeed
                                        #:on-error fail
                                        #:label [menu-label "Idris"])
  (-> (is-a?/c idris-highlighting-editor<%>)
      (is-a?/c has-idris<%>)
      #:on-success (->* (string?)
                        ((listof (list/c exact-nonnegative-integer? exact-nonnegative-integer? any/c)))
                        any/c)
      #:on-error (->* (string?)
                      ((listof (list/c exact-nonnegative-integer? exact-nonnegative-integer? any/c)))
                      any/c)
      (-> (or/c (is-a?/c menu%) (is-a?/c popup-menu%)
                (is-a?/c menu-bar%))
          void?))
  (lambda (parent)
    (define menu (new menu% [label menu-label] [parent parent]))
    void?))

(define/contract (add-idris-keys editor connection load-thunk #:on-success succeed #:on-error fail #:auto-load? auto-load?)
  (-> (is-a?/c idris-highlighting-editor<%>)
      (is-a?/c has-idris<%>)
      (-> void?)
      #:on-success (->* (string?)
                        ((listof (list/c exact-nonnegative-integer? exact-nonnegative-integer? any/c)))
                        any/c)
      #:on-error (->* (string?)
                      ((listof (list/c exact-nonnegative-integer? exact-nonnegative-integer? any/c)))
                      any/c)
      #:auto-load? any/c
      void?)
  (let ([keymap (send editor get-keymap)])
    (send* keymap
      ;; info commands
      (add-function "Get docs" (idris-name-command editor connection
                                                   ':docs-for #:on-success succeed #:on-error fail))
      (map-function "c:c;c:d" "Get docs")
      (add-function "Get type" (idris-name-command editor connection
                                                   ':type-of #:on-success succeed #:on-error fail))
      (map-function "c:c;c:t" "Get type")
      ;; editing commands
      (add-function "Load into Idris" (thunk* load-thunk))
      (map-function "c:c;c:l" "Load into Idris")
      (add-function "Start definition"
                    (idris-editing-command editor (list load-thunk) connection
                                           ':add-clause 'insert))
      (map-function "c:c;c:s" "Start definition")
      (add-function "Case split"
                    (idris-editing-command editor (list load-thunk) connection
                                           ':case-split 'replace))
      (map-function "c:c;c:c" "Case split")
      (add-function "Proof search"
                    (idris-editing-command editor (list load-thunk) connection
                                           ':proof-search 'replace-hole empty))
      (map-function "c:c;c:a" "Proof search"))
    (send editor set-keymap keymap)))
