#lang typed/racket

(provide (except-out (all-defined-out) Internal-Gemini-Path)
         gemini-path?)

(require/typed "gemini-path.rkt"
  [#:opaque Internal-Gemini-Path gemini-path?])

(define-type Gemini-Path (Refine [s : String] (: s Internal-Gemini-Path)))

(: string->gemini-path (-> String (Option Gemini-Path)))
(define (string->gemini-path path)
  (if (gemini-path? path) path #f))

; Fake nominal typing
(define-type Gemini-String (U String (U 'format 'insecure)))
(define-type Formatted-String (U String 'format))
(define-type Insecure-String (U String 'insecure))

