#lang info
(define collection "idris-interaction")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/idris-interaction.scrbl" ())))
(define pkg-desc "Interaction with Idris's IDE protocol, and useful GUI widgets built on that.")
(define version "0.0")

