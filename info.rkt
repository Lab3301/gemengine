#lang info
(define collection "gemengine")
(define deps '("typed-racket-lib"
               "typed-racket-more"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/gemengine.scrbl" ())))
(define pkg-desc "Gemini server")
(define version "0.3")
(define pkg-authors '(griffin))
