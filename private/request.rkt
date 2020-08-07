#lang typed/racket

(require typed/net/url "url-path.rkt")
(provide (all-defined-out))

(define-type QueryPair (Pairof Symbol (Option String)))

; Fake nominal typing
(define-type GeminiString (U String (U 'secure 'insecure)))
(define-type SecureString (U String 'secure))
(define-type InsecureString (U String 'insecure))

(struct request
  ([url : (Option URL)]
   [path : URL-Path]
   [query-string : InsecureString]
   [state : Symbol]))

(: empty-request (-> request))
(define (empty-request)
  (request #f (list "") "" 'fifty))

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

(: path/param->string (-> Path/Param String))
(define (path/param->string path-param)
  (let* ([path (path/param-path path-param)]
         [iparam (path/param-param path-param)]
         [param (if (empty? iparam) "" (string-join iparam ";" #:before-first ";"))])
    (format "~a~a" path param)))

(: url-query->string (-> (Listof QueryPair) String))
(define (url-query->string query-pairs)
  (: query-pair->string (-> QueryPair String))
  (define (query-pair->string pair)
    (let* ([first (car pair)]
           [val (cdr pair)]
           [second (if (false? val) "" (format "=~a" val))])
      (format "~a~a" first second)))

  (string-join (map query-pair->string query-pairs)))