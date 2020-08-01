#lang typed/racket

(require "request.rkt" "response.rkt" "url-path.rkt")
(provide (all-defined-out))

(struct route-trie ([value : (Option (-> request response))]
                    [table : (HashTable URL-Path-Segment route-trie)]) #:mutable)

(define routes : route-trie (route-trie #f (make-hash)))

(: route (-> String (-> request response) Void))
(define (route path body)
  (define url-path (string->url-path-with-wildcard path))
  (set-route routes url-path body))

(: set-route (-> route-trie URL-Path (-> request response) Void))
(define (set-route trie url-path body)
  (define path-segment (car url-path))
  (define path-tail (cdr url-path))
  (define table (route-trie-table trie))
  (define next (hash-ref table path-segment (lambda () (route-trie #f (make-hash)))))

  (hash-set! table path-segment next)

  (if (empty? path-tail)
      (set-route-trie-value! next body)
      (set-route next path-tail body)))

(: dispatch (-> URL-Path (-> request response)))
(define (dispatch url-path)
  (define body (get-route routes url-path))
  (if (false? body) not-found body))

(: get-route (-> route-trie URL-Path (Option (-> request response))))
(define (get-route trie url-path)
  (define path-segment (car url-path))
  (define path-tail (cdr url-path))
  (define table (route-trie-table trie))
  (define next (hash-ref table path-segment
                         (lambda () (wildcard-or-not-found table))))


  (if (empty? path-tail)
      (route-trie-value next)
      (get-route next path-tail)))

(: wildcard-or-not-found (-> (HashTable URL-Path-Segment route-trie) route-trie))
(define (wildcard-or-not-found trie-table)
  (hash-ref trie-table 'wildcard (lambda () (route-trie not-found (make-hash)))))

;; Default routes
(define not-found : (-> request response)
  (lambda ([_ : request]) (response "51 Not Found\r\n")))

