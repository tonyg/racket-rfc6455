#lang racket/base
;; RFC 6455 WebSocket connection management.

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
(require "frame.rkt")

(provide (struct-out ws-conn)
	 open-ws-conn?
	 ws-conn-line
	 ws-conn-closed?
	 ws-conn-headers
	 ws-send!
	 ws-recv
	 ws-close!
	 ws-stream-buffer-size)

(define ws-stream-buffer-size (make-parameter 65536))

(struct ws-conn ([closed? #:mutable] line headers ip op)
	#:property prop:evt (struct-field-index ip)
	#:transparent)

(define (open-ws-conn? v)
  (and (ws-conn? v)
       (not (ws-conn-closed? v))))

(define (stream-frames ip initial-opcode final-fragment? op)
  (define buffer (make-bytes (ws-stream-buffer-size)))
  (let loop ((opcode initial-opcode))
    (match (read-bytes-avail! buffer ip)
      [(? eof-object?)
       (when final-fragment?
	 (write-frame (ws-frame #t 0 #"") op))]
      [fragment-length
       (write-frame (ws-frame #f opcode (subbytes buffer 0 fragment-length)) op)
       (loop 0) ;; continuation
       ])))

(define (ws-send! c payload
		  #:final-fragment? [final-fragment? #t]
		  #:payload-type [payload-type 'text]
		  #:flush? [flush? #t])
  (unless (ws-conn-closed? c)
    (define opcode
      (match payload-type
	['continuation 0]
	['text 1]
	['binary 2]
	[_ (error 'ws-send! "Unsupported payload type: ~v" payload-type)]))
    (if (input-port? payload)
	(stream-frames payload opcode final-fragment? (ws-conn-op c))
	(let ((payload-bytes (cond
			      [(bytes? payload) payload]
			      [(string? payload) (string->bytes/utf-8 payload)]
			      [else (error 'ws-send! "Unsupported payload: ~v" payload)])))
	  (write-frame (ws-frame final-fragment? opcode payload-bytes)
		       (ws-conn-op c))))
    (when flush?
      (flush-output (ws-conn-op c)))))

(define (next-data-frame c)
  (match (read-frame (ws-conn-ip c))
    [(? eof-object? f) f]
    [(and f (ws-frame final? opcode payload))
     (case opcode
       [(0 1 2) ;; continuation, text, binary
	f]
       [(8) ;; close; shutdown
	(unless (ws-conn-closed? c)
	  (write-frame (ws-frame #t 8 #"") (ws-conn-op c))
	  (set-ws-conn-closed?! c #t))
	eof]
       [(9) ;; ping; reply
	(write-frame (ws-frame #t 10 payload) (ws-conn-op c))
	(next-data-frame c)]
       [(10) ;; unsolicited pong; ignore
	(next-data-frame c)]
       [else
	(ws-close! c #:status 1002 #:reason (format "Unexpected opcode ~a" opcode))
	eof])]))

(define (finish-non-stream-recv rev-acc converter)
  (converter (apply bytes-append (reverse rev-acc))))

(define (ws-recv c #:stream? [stream? #f] #:payload-type [payload-type 'auto])
  (flush-output (ws-conn-op c))
  (if stream?
      (let-values (((ip op) (make-pipe)))
	(thread (lambda ()
		  (let loop ()
		    (if (ws-conn-closed? c)
			(close-output-port op)
			(match (next-data-frame c)
			  [(? eof-object?) (close-output-port op)]
			  [(ws-frame final? opcode payload)
			   (write-bytes payload op)
			   (if final?
			       (close-output-port op)
			       (loop))])))))
	ip)
      (let loop ((rev-acc '()) (converter (case payload-type
					    [(text) bytes->string/utf-8]
					    [(binary) values]
					    [(auto) #f]
					    [else (error 'ws-recv
							 "Unsupported payload type: ~v"
							 payload-type)])))
	(if (ws-conn-closed? c)
	    (finish-non-stream-recv rev-acc converter)
	    (match (next-data-frame c)
	      [(? eof-object?) (finish-non-stream-recv rev-acc converter)]
	      [(ws-frame final? opcode payload)
	       ((if final? finish-non-stream-recv loop)
		(cons payload rev-acc)
		(or converter (case opcode
				[(1) bytes->string/utf-8]
				[(2) values]
				[else #f])))])))))

(define (ws-close! c #:status [status 1000] #:reason [reason ""])
  (unless (ws-conn-closed? c)
    (write-frame (ws-frame #t 8 (bytes-append (integer->integer-bytes status 2 #f #t)
					      (string->bytes/utf-8 reason)))
		 (ws-conn-op c))
    (set-ws-conn-closed?! c #t)
    (flush-output (ws-conn-op c))
    (let loop ()
      (unless (eof-object? (next-data-frame c))
	(loop)))))
