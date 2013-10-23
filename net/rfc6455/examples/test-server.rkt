#lang racket/base
;; Demonstrates the ws-serve interface.
;; Public Domain.

(require "../../rfc6455.rkt")

(define (connection-handler c state)
  (let loop ()
    (sync (handle-evt (alarm-evt (+ (current-inexact-milliseconds) 1000))
		      (lambda _
			(ws-send! c "Waited another second")
			(loop)))
	  (handle-evt c
		      (lambda _
			(define m (ws-recv c #:payload-type 'text))
			(unless (eof-object? m)
			  (if (equal? m "goodbye")
			      (ws-send! c "Goodbye!")
			      (begin (ws-send! c (format "You said: ~v" m))
				     (loop))))))))
  (ws-close! c))

(define stop-service
  (ws-serve #:port 8081 connection-handler))

(printf "Server running. Hit enter to stop service.\n")
(void (read-line))
(stop-service)
