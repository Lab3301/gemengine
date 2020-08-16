#lang racket

(require net/url)

(provide gemini-path?)

(define (gemini-path? url)
  (false? (url-host (string->url url))))
