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
(require "../conn-api.rkt")

(provide (struct-out rfc6455-conn)
	 rfc6455-stream-buffer-size)

(define rfc6455-stream-buffer-size (make-parameter 65536))

(define (stream-frames ip initial-opcode final-fragment? op)
  (define buffer (make-bytes (rfc6455-stream-buffer-size)))
  (let loop ((opcode initial-opcode))
    (match (read-bytes-avail! buffer ip)
      [(? eof-object?)
       (when final-fragment?
	 (write-frame (rfc6455-frame #t 0 #"") op))]
      [fragment-length
       (write-frame (rfc6455-frame #f opcode (subbytes buffer 0 fragment-length)) op)
       (loop 0) ;; continuation
       ])))

(define (rfc6455-send! c payload
		       #:final-fragment? [final-fragment? #t]
		       #:payload-type [payload-type 'text]
		       #:flush? [flush? #t])
  (unless (ws-conn-base-closed? c)
    (define opcode
      (match payload-type
	['continuation 0]
	['text 1]
	['binary 2]
	[_ (error 'rfc6455-send! "Unsupported payload type: ~v" payload-type)]))
    (if (input-port? payload)
	(stream-frames payload opcode final-fragment? (ws-conn-base-op c))
	(let ((payload-bytes (cond
			      [(bytes? payload) payload]
			      [(string? payload) (string->bytes/utf-8 payload)]
			      [else (error 'rfc6455-send! "Unsupported payload: ~v" payload)])))
	  (write-frame (rfc6455-frame final-fragment? opcode payload-bytes)
		       (ws-conn-base-op c))))
    (when flush?
      (flush-output (ws-conn-base-op c)))))

(define (next-data-frame c)
  (match (read-frame (ws-conn-base-ip c))
    [(? eof-object? f) f]
    [(and f (rfc6455-frame final? opcode payload))
     (case opcode
       [(0 1 2) ;; continuation, text, binary
	f]
       [(8) ;; close; shutdown
	(unless (ws-conn-base-closed? c)
	  (write-frame (rfc6455-frame #t 8 #"") (ws-conn-base-op c))
	  (set-ws-conn-base-closed?! c #t))
	eof]
       [(9) ;; ping; reply
	(write-frame (rfc6455-frame #t 10 payload) (ws-conn-base-op c))
	(next-data-frame c)]
       [(10) ;; unsolicited pong; ignore
	(next-data-frame c)]
       [else
	(rfc6455-close! c #:status 1002 #:reason (format "Unexpected opcode ~a" opcode))
	eof])]))

(define (finish-non-stream-recv rev-acc converter)
  (converter (apply bytes-append (reverse rev-acc))))

(define (rfc6455-recv c #:stream? [stream? #f] #:payload-type [payload-type 'auto])
  (flush-output (ws-conn-base-op c))
  (if stream?
      (let-values (((ip op) (make-pipe)))
	(thread (lambda ()
		  (let loop ()
		    (if (ws-conn-base-closed? c)
			(close-output-port op)
			(match (next-data-frame c)
			  [(? eof-object?) (close-output-port op)]
			  [(rfc6455-frame final? opcode payload)
			   (write-bytes payload op)
			   (if final?
			       (close-output-port op)
			       (loop))])))))
	ip)
      (let loop ((rev-acc '()) (converter (case payload-type
					    [(text) bytes->string/utf-8]
					    [(binary) values]
					    [(auto) #f]
					    [else (error 'rfc6455-recv
							 "Unsupported payload type: ~v"
							 payload-type)])))
	(if (ws-conn-base-closed? c)
	    eof
	    (match (next-data-frame c)
	      [(? eof-object?) eof]
	      [(rfc6455-frame final? opcode payload)
	       ((if final? finish-non-stream-recv loop)
		(cons payload rev-acc)
		(or converter (case opcode
				[(1) bytes->string/utf-8]
				[(2) values]
				[else #f])))])))))

(define (rfc6455-close! c #:status [status 1000] #:reason [reason ""])
  (unless (ws-conn-base-closed? c)
    (write-frame (rfc6455-frame #t 8 (bytes-append (integer->integer-bytes status 2 #f #t)
					      (string->bytes/utf-8 reason)))
		 (ws-conn-base-op c))
    (set-ws-conn-base-closed?! c #t)
    (flush-output (ws-conn-base-op c))
    (let loop ()
      (unless (eof-object? (next-data-frame c))
	(loop)))))

(struct rfc6455-conn ws-conn-base ()
	#:transparent
	#:methods gen:ws-conn
	[(define (ws-conn-supports-fragmentation? c) #t)
	 (define (ws-conn-supports-payload-type? c payload-type)
	   (memq payload-type '(text binary)))
	 (define (ws-conn-signals-status-on-close? c) #t)
	 (define (ws-conn-closed? c) (ws-conn-base-closed? c))
	 (define (ws-conn-line c) (ws-conn-base-line c))
	 (define (ws-conn-headers c) (ws-conn-base-headers c))
	 (define (ws-send! c payload
			   #:final-fragment? [final-fragment? #t]
			   #:payload-type [payload-type 'text]
			   #:flush? [flush? #t])
	   (rfc6455-send! c payload
			  #:final-fragment? final-fragment?
			  #:payload-type payload-type
			  #:flush? flush?))
	 (define (ws-recv c
			  #:stream? [stream? #f]
			  #:payload-type [payload-type 'text])
	   (rfc6455-recv c
			 #:stream? stream?
			 #:payload-type payload-type))
	 (define (ws-close! c
			    #:status [status 1000]
			    #:reason [reason ""])
	   (rfc6455-close! c
			   #:status status
			   #:reason reason))])