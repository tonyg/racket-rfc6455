#lang racket/base

(require racket/match)
(require racket/string)
(require web-server/http/request-structs)
(require web-server/private/connection-manager)
(require net/url)
(require (only-in openssl ssl-port?))

(provide tokenize-header-value
	 reconstruct-request-line
	 url->resource-string
	 construct-ws-location)

(define (tokenize-header-value v)
  (and v
       (map (lambda (p) (string-downcase (string-trim p)))
	    (string-split (bytes->string/latin-1 v) ","))))

(define (reconstruct-request-line req) ;; hmm.
  (bytes-append (request-method req)
		#" "
		(string->bytes/latin-1 (url->string (request-uri req)))
		#" HTTP/1.1"))

(define (url->resource-string u)
  (string-join (cons "" (map path/param-path (url-path u))) "/"))

;; Follows the rules in section 3.2 of draft-ietf-hybi-thewebsocketprotocol-00.txt
;; for constructing a URL for use in a Sec-WebSocket-Location header.
(define (construct-ws-location conn req)
  (define is-secure? (and (ssl-port? (connection-i-port conn))
			  (ssl-port? (connection-o-port conn))))
  
  (define host-header (headers-assq* #"Host" (request-headers/raw req)))
  (define default-port (if is-secure? 443 80))
  (define-values (host port)
    (match (string-split (if host-header (bytes->string/latin-1 (header-value host-header)) ""))
      ['() (values "" default-port)]
      [(list host) (values host default-port)]
      [(list host port-str) (values host (string->number port-str))]))
  (define resource (url->resource-string (request-uri req)))
  (string->bytes/latin-1
   (string-append (if is-secure? "wss://" "ws://")
		  host
		  (if (or (and is-secure? (not (equal? port 443)))
			  (and (not is-secure?) (not (equal? port 80))))
		      (string-append ":" (number->string port))
		      "")
		  resource)))
