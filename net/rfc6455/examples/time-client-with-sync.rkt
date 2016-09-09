#lang racket/base
;; Client for time-server.rkt.
;; Public Domain.

(require "../../rfc6455.rkt")
(require net/url)

(define (recv/print c)
  (printf "Got message: ~a\n" (ws-recv c)))

(printf "Connecting...\n")
(define c (ws-connect (string->url "ws://localhost:8081/")))
(printf "Connected. Press ENTER to quit.\n")

(ws-send! c "Hello from time-client-with-sync.rkt")
(let loop ()
  (sync (handle-evt c
                    (lambda _
                      (recv/print c)
                      (loop)))
        (handle-evt (current-input-port)
                    (lambda _
                      (unless (equal? (read-line) "")
                        (loop))))))

(printf "Disconnecting.\n")
(ws-close! c)
