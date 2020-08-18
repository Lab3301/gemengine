#lang typed/racket

(require (except-in typed/openssl ssl-listen) typed/net/url
         "private/url-path.rkt" "private/request.rkt"
         "private/response.rkt" "private/routes.rkt"
         "private/utils.rkt")

; See https://github.com/racket/typed-racket/pull/951 for why this is needed.
(require/typed openssl
  [ssl-listen (->* (Exact-Positive-Integer)
                   (Exact-Nonnegative-Integer Boolean
                                              (Option String)
                                              (U SSL-Server-Context SSL-Protocol))
                   SSL-Listener)])

(provide serve serve-forever route (struct-out request)
         response (all-from-out "private/utils.rkt"))

(: serve (-> Positive-Integer String String Positive-Integer Positive-Integer (-> Void)))
(define (serve port-no cert key timeout mem-limit)
  (define main-cust (make-custodian))

  (parameterize ([current-custodian main-cust])
    (define ctx (ssl-make-server-context 'tls12))

    (ssl-load-certificate-chain! ctx cert)
    (ssl-load-private-key! ctx key)

    (define listener : SSL-Listener (ssl-listen port-no 5 #t #f ctx))

    (define (loop) : Void
      (accept-and-handle listener timeout mem-limit)
      (loop))

    (thread loop))

  (lambda ()
    (custodian-shutdown-all main-cust)))

(: serve-forever (->* (Positive-Integer)
                      (#:certificate String
                       #:key String
                       #:timeout Positive-Integer
                       #:memory-limit Positive-Integer)
                      (-> Void)))
(define (serve-forever port-no
                       #:certificate [cert "/etc/gemini/certificate.pem"]
                       #:key [key "/etc/gemini/key.pem"]
                       #:timeout [timeout 10]
                       #:memory-limit [mem-limit 10])

  (serve port-no cert key timeout mem-limit)

  (define (loop)
    (sync never-evt))

  (loop))

(: accept-and-handle (-> SSL-Listener Positive-Integer Positive-Integer Thread))
(define (accept-and-handle listener timeout mem-limit)
  (define cust (make-custodian))

  (custodian-limit-memory cust (* mem-limit 1024 1024))

  (parameterize ([current-custodian cust])
    (with-handlers ([exn:fail:network? (lambda (_) (displayln "Failed TLS connection."))])
      (define-values (in out) (ssl-accept listener))
      (thread (lambda ()
                (handle in out)
                (close-input-port in)
                (close-output-port out)))))

  (thread (lambda ()
            (sleep timeout)
            (custodian-shutdown-all cust))))

(: handle (-> Input-Port Output-Port Void))
(define (handle in out)
  (define req : request (port->request in))
  (define output : String (handle-req req))
  (display output out))

(: handle-req (-> request String))
(define (handle-req req)
  (define req-body : (-> request response)
    (dispatch req))

  (response-body (req-body req)))
