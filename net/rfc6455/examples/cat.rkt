#lang racket/base
;; Simple pass-through cat-like client.
;; Public Domain.

(module+ main
  (require "../../rfc6455.rkt")
  (require net/url)
  (require racket/cmdline)
  (require (only-in racket/port read-line-evt))

  (define server "ws://localhost:8081/")
  (command-line #:once-each
                ["--server" s
                 "URL of Websocket server to contact"
                 (set! server s)])

  (define c (ws-connect (string->url server)))
  (let loop ()
    (sync (handle-evt (read-line-evt (current-input-port) 'any)
                      (lambda (line)
                        (if (eof-object? line)
                            (void)
                            (begin (ws-send! c line)
                                   (loop)))))
          (handle-evt (ws-recv-evt c #:payload-type 'text)
                      (lambda (line)
                        (if (eof-object? line)
                            (void)
                            (begin (printf "~a\n" line)
                                   (loop)))))))
  (ws-close! c))
