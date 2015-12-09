#lang racket

(require racket/gui)
(provide idris-thread)

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

  (define idris-executable
    (if (eq? (system-type 'os) 'windows)
          "idris.exe"
          "idris"))
  
  (let-values (((proc out in err)
                (subprocess #f #f #f (find-executable-path idris-executable) "--ide-mode-socket")))

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

     (define (call-handler handler msg [highlights empty])
       (when handler
         (if (cons? highlights)
             (handler msg (car highlights))
             (handler msg))))

     (define (update)
       (let ([reply (idris-receive idris)])
         (when reply (displayln (format "Receiving ~a" reply)))
         (match reply
             [(list ':return (list-rest ':ok msg highlights) request-id)
              (call-handler (hash-ref success-continuations request-id #f) msg highlights)
              (done-with-request-id request-id)
              (update)]
             [(list ':return (list-rest ':error msg highlights) request-id)
              (call-handler (hash-ref error-continuations request-id #f) msg highlights)
              (done-with-request-id request-id)
              (update)]
             [(list ':output (list-rest ':ok msg highlights) request-id)
              (call-handler (hash-ref progress-continuations request-id #f) msg highlights)
              (update)]
             [(list ':output (list-rest ':error msg highlights) request-id)
              (call-handler (hash-ref error-continuations request-id #f) msg highlights)
              (update)]
             [(list ':write-string str request-id)
              (call-handler (hash-ref progress-continuations request-id #f)
                            reply)
              (update)]
             [(list ':set-prompt new-prompt request-id)
              (call-handler (hash-ref progress-continuations request-id #f)
                            reply)
              (update)]
             [(list ':warning warning request-id)
              (call-handler (hash-ref progress-continuations request-id #f)
                            reply)
              (update)]
             [#f void]
             [other (printf "Didn't understand Idris message ~a" other)])))

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
                  (displayln (format "Sending ~s" sexp))
                  (idris-send idris (list sexp request-id)))
                (go)])
             (begin (update)
                    (go))))))))


