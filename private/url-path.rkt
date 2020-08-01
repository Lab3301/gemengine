#lang typed/racket

(require typed/net/url)
(provide (all-defined-out))

(define-type URL-Path-Segment (U 'up 'same 'wildcard String))
(define-type URL-Path (Listof URL-Path-Segment))

(: string->url-path (-> String URL-Path))
(define (string->url-path path)
  (map path/param-path (url-path (string->url path))))

(: string->url-path-with-wildcard (-> String URL-Path))
(define (string->url-path-with-wildcard path)
  (map path-with-wildcard (url-path (string->url path))))

(: path-with-wildcard (-> Path/Param URL-Path-Segment))
(define (path-with-wildcard path)
  (define segment (path/param-path path))
  (if (and (string? segment) (string-prefix? segment ":"))
      'wildcard
      segment))
