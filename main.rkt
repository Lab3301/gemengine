#lang typed/racket

(require typed/openssl typed/net/url
         "private/url-path.rkt" "private/request.rkt"
         "private/response.rkt" "private/routes.rkt"
         "private/utils.rkt")

(provide serve serve-forever route (struct-out request)
         response (all-from-out "private/utils.rkt"))

(: serve (-> Positive-Integer String String (-> Void)))
(define (serve port-no cert key)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener : SSL-Listener (ssl-listen port-no 5 #t))

    (define (loop) : Void
      ;; Generated with:
      ;;
      ;; openssl req -newkey rsa:2048 -nodes -keyout key.pem -x509 -days 365 -out certificate.pem
      (ssl-load-certificate-chain! listener cert)
      (ssl-load-private-key! listener key)

      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(: serve-forever (->* (Positive-Integer) (#:certificate String #:key String) (-> Void)))
(define (serve-forever port-no #:certificate [cert "/etc/gemini/certificate.pem"] #:key [key "/etc/gemini/key.pem"])
  (serve port-no cert key)

  (define (loop)
    (sync never-evt))

  (loop))

(: accept-and-handle (-> SSL-Listener Thread))
(define (accept-and-handle listener)
  (define cust (make-custodian))

  (parameterize ([current-custodian cust])
    (define-values (in out) (ssl-accept listener))
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))

  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(: handle (-> Input-Port Output-Port Void))
(define (handle in out)
  (define req : request (port->request in))
  (define output : String (handle-req req))
  (display output out))

(: handle-req (-> request String))
(define (handle-req req)
  (define req-body : (-> request response)
    (dispatch (request-path req)))

  (response-body (req-body req)))
