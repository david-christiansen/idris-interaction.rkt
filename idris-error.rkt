#lang racket/base

(provide (struct-out idris-error))

(struct idris-error
  (filename
   start-line start-column
   end-line end-column
   text
   highlighting)
  #:transparent)
