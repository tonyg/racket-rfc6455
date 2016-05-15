#lang racket/base

(provide (struct-out ws-conn-base)
	 open-ws-conn?

	 gen:ws-conn
	 ws-conn?
	 ws-conn-supports-fragmentation?
	 ws-conn-supports-payload-type?
	 ws-conn-signals-status-on-close?
	 ws-conn-closed?
	 ws-conn-line
	 ws-conn-headers
	 ws-send!
	 ws-recv
	 ws-close!

         ws-conn-peer-addresses
         ws-conn-host+port
         ws-conn-path)

(require racket/generic)
(require racket/match)
(require racket/string)
(require web-server/http/request-structs)

(struct ws-conn-base ([closed? #:mutable] line headers ip op bump-timeout!)
	#:property prop:evt (struct-field-index ip)
	#:transparent)

(define (open-ws-conn? x)
  (and (ws-conn? x)
       (not (ws-conn-closed? x))))

(define-generics ws-conn
  (ws-conn-supports-fragmentation? ws-conn)
  (ws-conn-supports-payload-type? ws-conn payload-type)
  (ws-conn-signals-status-on-close? ws-conn)
  (ws-conn-closed? ws-conn)
  (ws-conn-line ws-conn)
  (ws-conn-headers ws-conn)
  (ws-send! ws-conn payload
	    #:final-fragment? [final-fragment?]
	    #:payload-type [payload-type]
	    #:flush? [flush?])
  (ws-recv ws-conn
	   #:stream? [stream?]
	   #:payload-type [payload-type])
  (ws-close! ws-conn
	     #:status [status]
	     #:reason [reason]))

(define (ws-conn-peer-addresses c)
  (local-require racket/tcp)
  (local-require openssl)
  (define ip (ws-conn-base-ip c))
  (if (ssl-port? ip)
      (ssl-addresses ip #t)
      (tcp-addresses ip #t)))

;; WSConn -> (Values (Option Bytes) (Option Natural))
(define (ws-conn-host+port c)
  (cond [(headers-assq* #"Host" (ws-conn-headers c)) =>
         (lambda (h)
           (match (header-value h)
             [(regexp #px"(.*):(\\d+)" (list _ host port))
              (values host (string->number (bytes->string/latin-1 port)))]
             [host
              (values host #f)]))]
        [else
         (values #f #f)]))

(define (bytes-split bs)
  (map string->bytes/latin-1
       (string-split (bytes->string/latin-1 bs))))

;; WSConn -> Bytes
(define (ws-conn-path c)
  (match-define (list* _method path _rest) (bytes-split (ws-conn-line c)))
  path)
