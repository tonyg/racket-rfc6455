#lang racket/base

(require racket/generic)

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
	 ws-close!)

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
