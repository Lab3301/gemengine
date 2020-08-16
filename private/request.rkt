#lang typed/racket

(require typed/net/url "url-path.rkt" "types.rkt")

(provide (all-defined-out))

(define-type QueryPair (Pairof Symbol (Option String)))

(struct request
  ([url : (Option URL)]
   [path : URL-Path]
   [query-string : Insecure-String]
   [state : (U 'ok 'err)]))

(: empty-request (-> request))
(define (empty-request)
  (request #f (list "") "" 'err))

(: port->request (-> Input-Port request))
(define (port->request in)
  (define line (read-line in))

  (if (eof-object? line)
      (empty-request)
      (string->request (string-trim line))))

(: string->request (-> String request))
(define (string->request line)
  (define url (string->url line))
  (define path : URL-Path
    (map path/param->string (url-path url)))
  (define query : String (url-query->string (url-query url)))

  (request url path query 'ok))

(: url-query->string (-> (Listof QueryPair) String))
(define (url-query->string query-pairs)
  (: query-pair->string (-> QueryPair String))
  (define (query-pair->string pair)
    (let* ([first (car pair)]
           [val (cdr pair)]
           [second (if (false? val) "" (format "=~a" val))])
      (format "~a~a" first second)))

  (string-join (map query-pair->string query-pairs)))
