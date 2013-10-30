#lang racket/base
;; RFC 6455 Handshaking & connection establishment.

;; Copyright (c) 2013 Tony Garnock-Jones
;;
;; This module is distributed under the GNU Lesser General Public
;; License (LGPL). This means that you can link it into proprietary
;; applications, provided you follow the rules stated in the LGPL. You
;; can also modify this module; if you distribute a modified version,
;; you must distribute it under the terms of the LGPL, which in
;; particular means that you must release the source code for the
;; modified software. See http://www.gnu.org/licenses/lgpl-3.0.txt for
;; more information.

(require rackunit)
(require file/sha1)
(require net/base64)
(require web-server/private/connection-manager)
(require web-server/dispatchers/dispatch)
(require web-server/http/request-structs)
(require "conn.rkt")
(require "../http.rkt")
(require "../timeout.rkt")

(provide make-rfc6455-dispatcher)

(define (key-digest key)
  (base64-encode (sha1-bytes (open-input-bytes
			      (bytes-append key #"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
		 #""))

;; Example from rfc6455, section 4.2.2 
(check-equal? (key-digest #"dGhlIHNhbXBsZSBub25jZQ==") #"s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")

(define (make-rfc6455-dispatcher conn-dispatch [conn-headers #f])
  (lambda (conn req)
    (define headers (request-headers/raw req))
    (define (get-header k) (define h (headers-assq* k headers)) (and h (header-value h)))
    (define (header-equal? k expected)
      (define actual (get-header k))
      (and actual (string-ci=? (bytes->string/latin-1 actual) expected)))
    (define websocket-key (get-header #"Sec-WebSocket-Key"))
    (when (or (not websocket-key)
	      (not (header-equal? #"Sec-WebSocket-Version" "13"))
	      (not (member "websocket" (tokenize-header-value (get-header #"Upgrade"))))
	      (not (member "upgrade" (tokenize-header-value (get-header #"Connection")))))
      (next-dispatcher))
    (define request-line (reconstruct-request-line req))

    (define-values (reply-headers connection-state)
      (if conn-headers
	  (if (procedure-arity-includes? conn-headers 3)
	      (conn-headers request-line headers req)
	      (conn-headers request-line headers))
	  (values '() (void))))

    (define op (connection-o-port conn))
    (define (output-header h) (fprintf op "~a: ~a\r\n" (header-field h) (header-value h)))

    (fprintf op "HTTP/1.1 101 Switching Protocols\r\n")
    (output-header (header #"Upgrade" #"websocket"))
    (output-header (header #"Connection" #"Upgrade"))
    (output-header (header #"Sec-WebSocket-Accept" (key-digest websocket-key)))
    (for ((h reply-headers)) (output-header h))
    (fprintf op "\r\n")
    (flush-output op)

    (bump-connection-timeout! conn)
    (conn-dispatch (rfc6455-conn #f
				 request-line
				 headers
				 (connection-i-port conn)
				 op
				 (lambda () (bump-connection-timeout! conn)))
		   connection-state)))
