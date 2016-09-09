#lang racket/base
;; Time server, demonstrating the ws-serve interface.
;; Public Domain.

(require "../../rfc6455.rkt")

(define next-connection-id 0)

(define (connection-handler c state)
  (define id next-connection-id)
  (set! next-connection-id (+ next-connection-id 1)) ;; not thread safe
  (printf "Connection received: ~v\n" id)
  (let loop ((deadline (current-inexact-milliseconds)))
    (sync (handle-evt (alarm-evt deadline)
		      (lambda _
			(ws-send! c (number->string deadline))
			(loop (+ deadline 1000))))
	  (handle-evt c
		      (lambda _
			(define m (ws-recv c #:payload-type 'text))
			(unless (eof-object? m)
                          (printf "Ignoring message ~v received from ~v\n" m id)
                          (loop deadline))))))
  (ws-close! c)
  (printf "Connection closed: ~v\n" id))

(define stop-service
  (ws-serve #:port 8081 connection-handler))

(printf "Server running. Hit enter to stop service.\n")
(void (read-line))
(stop-service)
