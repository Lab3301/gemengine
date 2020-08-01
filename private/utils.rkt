#lang typed/racket

(require "request.rkt" "response.rkt" "routes.rkt" typed/net/url)

(provide (all-defined-out))

(: send-file (-> SecureString response))
(define (send-file filename)
  (assert filename string?)

  (if (file-exists? filename)
      (send-text (file->string filename))
      (not-found (empty-request))))

(: send-text (-> SecureString response))
(define (send-text content)
  (response (format "20 text/gemini\r\n~a" content)))

(: request-input (-> SecureString response))
(define (request-input prompt)
  (response (format "10 ~a\r\n" prompt)))

(: redirect (-> SecureString response))
(define (redirect url)
  (response (format "30 ~a\r\n" url)))

(: escape (-> GeminiString SecureString))
(define (escape text)
  (assert text string?)

  (cond
    [(zero? (string-length text)) text]
    [(special-char-prefix? text) (format "\\~a" text)]
    [else text]))

(: special-char-prefix? (-> String Boolean))
(define (special-char-prefix? text)
  (define special-chars : (Listof String)
    (list "#" "* " "=> " "```" ">"))

  (ormap
   (lambda ([prefix : String])
     (string-prefix? text prefix))
   special-chars))
