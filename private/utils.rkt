#lang typed/racket

(require "request.rkt" "response.rkt" "routes.rkt"
         "types.rkt" typed/net/url)

(provide (all-defined-out)
         (all-from-out "types.rkt"))

(define file-dir : (Boxof (Option Path)) (box #f))

(: set-file-directory (-> Path Void))
(define (set-file-directory dir)
  (if (string? (unbox file-dir))
      (raise "Cannot set file directory multiple times.")
      (set-box! file-dir [simplify-path (path->complete-path dir)])))

(: send-file (-> Path-String response))
(define (send-file filename)
  (define dir : (Option Path) (unbox file-dir))

  (if (path? dir)
      (let ([file (path->string [simplify-path (build-path dir filename)])])
        (cond
          [(not (string-prefix? file (path->string dir)))
           (temporary-failure (empty-request))]
          [(file-exists? file)
           (send-text (file->string file))]
          [else (not-found (empty-request))]))
      (raise "File directory must be set.")))

(: send-text (-> Formatted-String response))
(define (send-text content)
  (response (format "20 text/gemini\r\n~a" content)))

(: request-input (-> Formatted-String response))
(define (request-input prompt)
  (response (format "10 ~a\r\n" prompt)))

(: redirect (-> Gemini-Path response))
(define (redirect url)
  (redirect-external url))

(: redirect-external (-> Gemini-String response))
(define (redirect-external url)
  (response (format "30 ~a\r\n" url)))

(: escape (-> Gemini-String Formatted-String))
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
