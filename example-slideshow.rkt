#lang slideshow

(require (only-in idris-interaction/idris-interaction
                  idris-load-packages idris-flags))
(require idris-interaction/idris-slideshow-repl)

;; Set the -p argument to Idris
(idris-load-packages '("pruviloj" "contrib"))

;; Set additional command-line flags
(idris-flags '("--no-partial-eval"))


(slide
 (titlet "Demo of Idris Slides"))

;; Exhibit the output of Idris commands interactively in a slide
(slide #:title "The Elaborator: Input and Output"
         (idris-slideshow-commands
          (list (list "Vect.(++) in Idris" ":printdef Vect.(++)")
                (list "Vect.(++) in TT" ":core Vect.(++)"))
          #:face "DejaVu Sans Mono"
          ;; Font size
          #:size 32
          ;; This is the module context within which the commands are executed
          #:preamble "module Slide\n\nimport Data.Vect\n\n"
          #:width 1000))

;; Put Idris code in a slide. The keybindings in the slide editor are
;; a subset of those in idris-mode for Emacs.
(slide #:title "Interactive Idris stuff"
       (idris-slideshow-editor
        (string-join
         '("foo : Nat -> Nat"
           "foo x = ?huh"
           ""
           "two : Fin 3"
           "two = ?what")
         "\n")
        #:face "DejaVu Sans Mono"
        #:size 25
        ;; Show an interactive REPL in addition to the code editor
        #:repl? #t
        #:preamble "import Data.Fin"
        #:auto-load? #t))

(slide
 (titlet "Demo done!"))