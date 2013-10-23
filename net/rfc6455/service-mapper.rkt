#lang racket/base
;; Convenience interface for starting a more complex WebSocket
;; service, with multiple entry points (URL paths) and multiple
;; subprotocol handlers.

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

(require racket/match)
(require racket/string)
(require net/url)
(require web-server/http/request-structs)
(require web-server/dispatchers/dispatch)

(require "dispatcher.rkt")

(provide ws-service-mapper
	 make-service-mapper-dispatcher)

(define-syntax ws-service-mapper
  (syntax-rules ()
    [(_ [uri-regexp [(protocol ...) function] ...] ...)
     (lambda (uri)
       (define path (string-join (cons "" (map path/param-path (url-path uri))) "/"))
       (cond
	[(regexp-match-exact? uri-regexp path)
	 (lambda (requested-protocol)
	   (or (case requested-protocol [(protocol ...) function] [else #f])
	       ...))]
	...
	[else
	 (lambda (requested-protocol) #f)]))]))

(define (lookup-service service-mapper)
  (lambda (request-line headers req)
    (define protocol-header (headers-assq* #"Sec-WebSocket-Protocol" headers))
    (define requested-protocols (if protocol-header
				    (map string->symbol
					 (tokenize-header-value (header-value protocol-header)))
				    (list #f))) ;; signifies the absence of the header
    (define protocol-mapper (service-mapper (request-uri req)))
    (define-values (selected-protocol function)
      (let loop ((remaining requested-protocols))
	(match remaining
	  ['() (next-dispatcher)]
	  [(cons selected-protocol rest)
	   (define maybe-function (protocol-mapper selected-protocol))
	   (if maybe-function
	       (values selected-protocol maybe-function)
	       (loop rest))])))
    (values (if selected-protocol
		(list (header #"Sec-WebSocket-Protocol"
			      (string->bytes/latin-1 (symbol->string selected-protocol))))
		'())
	    function)))

(define (make-service-mapper-dispatcher service-mapper)
  (make-rfc6455-dispatcher (lambda (c selected-service-handler) (selected-service-handler c))
			   #:conn-headers (lookup-service service-mapper)))
