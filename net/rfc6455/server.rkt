#lang racket/base
;; Convenience interface for starting a simple web service.
;; Roughly compatible with net/websocket/server's ws-serve.

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
(require web-server/web-server)
(require web-server/http/request-structs)
(require web-server/http/response)
(require web-server/http/response-structs)
(require web-server/dispatchers/dispatch)
(require "dispatcher.rkt")
(require "service-mapper.rkt")
(require "conn.rkt")

(provide ws-serve
	 ws-serve*
	 (except-out (all-from-out "conn.rkt") ws-conn set-ws-conn-closed?!))

(define (transpose xss) (apply map list xss))

(define (guard-dispatcher d)
  (lambda (conn req)
    (with-handlers [(exn:dispatcher?
		     (lambda (e)
		       (output-response/method
			conn
			(response 400 #"Bad WebSocket request" (current-seconds) #f '() void)
			(request-method req))))]
      (d conn req))))

(define ws-serve
  (procedure-rename
   (make-keyword-procedure
    (lambda (keys vals conn-dispatch . rest)
      (define kvs (map list keys vals))
      (define conn-headers-cell (assq '#:conn-headers kvs))
      (define conn-headers (and conn-headers-cell (cadr conn-headers-cell)))
      (define dispatcher (make-rfc6455-dispatcher conn-dispatch #:conn-headers conn-headers))
      (match-define (list keys1 vals1) (transpose (remq conn-headers-cell kvs)))
      (keyword-apply serve
		     (cons '#:dispatch keys1)
		     (cons (guard-dispatcher dispatcher) vals1)
		     rest)))
   'ws-serve))

(define ws-serve*
  (procedure-rename
   (make-keyword-procedure
    (lambda (keys vals service-mapper . rest)
      (keyword-apply serve
		     (cons '#:dispatch keys)
		     (cons (guard-dispatcher (make-service-mapper-dispatcher service-mapper)) vals)
		     rest)))
   'ws-serve*))
