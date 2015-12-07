#lang racket

(require racket/gui)

(struct idris-compiler
  (process
   stdin stdout stderr
   ide-input ide-output
   [ide-output-buffer #:auto #:mutable])
  #:auto-value #"")


(define (start-idris [debug #f])
  (define (close-ports . ports)
    (for ([p ports])
      (if (input-port? p)
          (close-input-port p)
          (close-output-port p))))

  (define (no-buffering . ports)
    (for ([p ports])
      (file-stream-buffer-mode p 'none)))

  (let-values (((proc out in err)
                (subprocess #f #f #f (find-executable-path "idris") "--ide-mode-socket")))

    (unless (equal? (subprocess-status proc) 'running)
      (close-ports in out err)
      (error "Failed to run process\n"))
    (let* ((str (read-line out 'any))
           (port (string->number str)))
      (unless port
        (close-ports in out err)
        (subprocess-kill proc)
        (error "failed to connect, string was " str))
      (when debug (printf "Connecting to ~a\n" port))
      (let-values (((ide-out ide-in)
                    (tcp-connect "localhost" port)))
        (when debug (printf "Connected to port ~ań" port))
        (no-buffering in out err ide-in ide-out)
        (idris-compiler proc in out err ide-in ide-out)))))

(define (kill-idris idris)
  (subprocess-kill (idris-compiler-process idris) #t)
  (close-output-port (idris-compiler-stdin idris))
  (close-input-port (idris-compiler-stdout idris))
  (close-input-port (idris-compiler-stderr idris)))

;;; Move stuff from the Idris sexp protocol output stream to the buffer
(define (idris-get-output! idris)
  (let ((ide-out (idris-compiler-ide-output idris)))
    (let get-all-bytes ()
      (let* ((buffer (idris-compiler-ide-output-buffer idris))
             (temp-buffer (make-bytes 256 0))
             (read-count (read-bytes-avail!* temp-buffer ide-out)))
        (cond ((eof-object? read-count)
               ;; We hit the EOF, no more to read
               void)
              ((procedure? read-count)
               (error "can't deal with this"))
              (else ;; it's a nonnegative integer
               ;; TODO: use a single buffer with fill pointers
               ;; instead of all this copying nonsense
               (when (> read-count 0)
                 (set-idris-compiler-ide-output-buffer!
                  idris
                  (bytes-append buffer
                                (subbytes temp-buffer 0 read-count)))
                 (get-all-bytes))))))))

(define (idris-receive idris)
  (idris-get-output! idris)

  (let* ((output-buffer (idris-compiler-ide-output-buffer idris))
         (message-length (if (>= (bytes-length output-buffer) 6)
                             (string->number (bytes->string/utf-8 output-buffer #f 0 6)
                                             16)
                             #f))
         (has-message? (and message-length
                            (>= (bytes-length output-buffer) (+ message-length 6)))))
    (if has-message?
        (let* ((message-string (bytes->string/utf-8 output-buffer #f 6 (+ 6 message-length)))
               (message-sexp (read (open-input-string message-string 'idris-message))))
          (set-idris-compiler-ide-output-buffer! idris
                                                 (subbytes output-buffer
                                                           (+ 6 message-length)))
          message-sexp)
        #f)))

(define (idris-send idris sexp-command)
  ;; 48 is UTF-8 for #\0
  (define zero-char 48)

  (let* ((message-string (format "~s" sexp-command))
         (message-bytes (string->bytes/utf-8 message-string))
         (length (string->bytes/utf-8
                  (number->string (+ (bytes-length message-bytes) 1)
                                  16)))
         (padding (make-bytes (- 6 (bytes-length length))
                              zero-char))
         (port (idris-compiler-ide-input idris)))
    (write-bytes padding port)
    (write-bytes length port)
    (write-bytes message-bytes port)
    (write-bytes #"\n" port)
    (flush-output port)))


(define (idris-thread)
  (thread
   (λ ()
     (define idris (start-idris))
     ;; Check protocol version
     (let get-protocol ()
       (let ((response (idris-receive idris)))
         (if response
             (unless (equal? response '(:protocol-version 1 0))
               (error "Wrong Idris protocol version" response))
             (get-protocol))))

     (define continuation-counter 0)
     (define success-continuations (make-hash))
     (define progress-continuations (make-hash))
     (define error-continuations (make-hash))

     (define (done-with-request-id request-id)
       (for ([cont (list success-continuations
                         progress-continuations
                         error-continuations)])
         (hash-remove! cont request-id)))

     (define (update)
       (match (idris-receive idris)
         [(list ':return (list-rest ':ok msg highlights) request-id)
          (let ((continuation (hash-ref success-continuations request-id #f)))
            (when continuation (continuation msg)))
          (done-with-request-id request-id)
          (update)]
         [(list ':return (list-rest ':error msg highlights) request-id)
          (let ((continuation (hash-ref error-continuations request-id #f)))
            (when continuation (continuation msg)))
          (done-with-request-id request-id)
          (update)]
         [(list ':output (list-rest ':ok msg highlights) request-id)
          (let ((continuation (hash-ref progress-continuations request-id #f)))
            (when continuation (continuation msg)))
          (update)]
         [(list ':output (list-rest ':error msg highlights) request-id)
          (let ((continuation (hash-ref error-continuations request-id #f)))
            (when continuation (continuation msg)))
          (update)]
         [(list ':write-string str)
          (displayln str)
          (update)]
         [(list ':set-prompt new-prompt)
          (printf "New prompt: ~a\n" new-prompt)
          (update)]
         [(list ':warning warning)
          (printf "Warning: ~a\n" warning)
          (update)]
         [#f void]
         [other (printf "Didn't understand Idris message ~a" other)]))

     (let go ()
       (let* ((thread-event (thread-receive-evt))
              (port-event (idris-compiler-ide-output idris))
              (evt (sync thread-event port-event)))
         (if (equal? evt thread-event)
             (match (thread-receive)
               ['quit
                (kill-idris idris)]
               ['update
                (update)
                (go)]
               [(list-rest 'send sexp on-success conts)
                (let ((request-id continuation-counter))
                  (set! continuation-counter (+ continuation-counter 1))
                  (hash-set! success-continuations request-id on-success)
                  (when (cons? conts)
                    (hash-set! progress-continuations request-id (car conts))
                    (when (cons? (cdr conts))
                      (hash-set! error-continuations request-id (cadr conts))))
                  (idris-send idris (list sexp request-id)))
                (go)])
             (begin (update) (go))))))))

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
    (inherit insert last-position get-text)

    (define input-beginning-position 0)
    (define current-prompt "")
    (define locked? #t)

    (define/public (set-prompt new-prompt)
      (set! current-prompt new-prompt))

    (define/augment (can-insert? start len)
      (and (>= start input-beginning-position)
           (not locked?)))

    (define/override (on-char c)
      (super on-char c)
      (when (and (eq? (send c get-key-code)
                      #\return)
                 (not locked?))
        (set! locked? #t)
        (eval-callback
         (get-text input-beginning-position
                   (- (last-position) 1)))))

    (define/public (insert-prompt)
      (queue-idris-output
       (λ ()
         (set! locked? #f)
         (insert current-prompt)
         (insert "> ")
         (set! input-beginning-position (last-position)))))

    (define/public (output str)
      (queue-idris-output
       (lambda ()
         (let ((was-locked? locked?))
           (set! locked? #f)
           (insert str)
           (set! locked? was-locked?)))))
    (super-new)))

(define idris-repl%
  (class frame%
    (field (my-idris-thread #f))

    (super-new [label "Idris"])

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
                                  ,(lambda (res)
                                     (send output-editor output res)
                                     (send output-editor output "\n")
                                     ;; this is the success cont, so we can insert
                                     ;; a prompt here
                                     (send output-editor insert-prompt))
                                  ,void
                                  ,(lambda (res)
                                     (send output-editor output res)
                                     (send output-editor output "\n")
                                     ;; this is the error cont, so we
                                     ;; should also give a prompt
                                     (send output-editor insert-prompt)
                                     ))))]))
    (define output (new editor-canvas% [parent panel] [editor output-editor]))))


(define (repl)
  (define frame (new idris-repl%))
  (send frame show #t))

