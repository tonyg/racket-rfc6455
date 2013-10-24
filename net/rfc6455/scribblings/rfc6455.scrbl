#lang scribble/manual

@(require scribble/manual
	  (for-label racket
		     net/url
		     web-server/http/request-structs
		     (prefix-in builtin: net/websocket/server)
		     net/rfc6455))

@title{RFC 6455 WebSockets for Racket}
@author[(author+email "Tony Garnock-Jones" "tonygarnockjones@gmail.com")]

@local-table-of-contents[]

@section{Introduction}

This package, @tt{rfc6455}, provides an
@link["http://tools.ietf.org/html/rfc6455"]{RFC 6455} compatible
WebSockets @emph{server} interface for Racket, building on Racket's
@racket[web-server] collection. Racket's built-in
@racket[net/websocket] collection implements an old draft of the
WebSockets protocol that is no longer supported by modern browsers,
whereas RFC 6455 is the standard. Wikipedia has a
@link["http://en.wikipedia.org/wiki/WebSocket#Browser_support"]{good
section on browser support for WebSocket protocol variations}, which
states "All the latest browsers except Android browser support the
latest specification (RFC 6455) of the WebSocket protocol."

In particular, this package has been tested against

@itemize[
  @item{Firefox 24.0}
  @item{Chrome 30.0.1599.101}
]

It does not work in Safari 5.x; only Safari 6.0 and newer have support
for RFC 6455 WebSockets. In principle, it ought to be possible to
combine this package's implementation with a fallback to the older
protocol variant implemented by @racketmodname[net/websocket/server],
but I have not explored this.

@section{Synopsis}

Using the @racketmodname[net/websocket]-compatible interface:

@racketblock[
  (require net/rfc6455)
  (ws-serve #:port 8081 (lambda (c s) (ws-send! c "Hello world!")))]

Using an interface that supports URL path matching and WebSocket
subprotocol selection:

@racketblock[
  (require net/rfc6455)
  (ws-serve* #:port 8081
	     (ws-service-mapper
	      ["/test" (code:comment "the URL path (regular expression)")
	       [(subprotocol) (code:comment "if client requests subprotocol \"subprotocol\"")
		(lambda (c) (ws-send! c "You requested a subprotocol"))]
	       [(#f) (code:comment "if client did not request any subprotocol")
		(lambda (c) (ws-send! c "You didn't explicitly request a subprotocol"))]]))]

@section{License}

The following text applies to all the code in this package except
files marked "public domain" in the @tt{net/rfc6455/examples} directory:

Copyright (c) 2013 Tony Garnock-Jones.

This package is distributed under the GNU Lesser General Public
License (LGPL). This means that you can link it into proprietary
applications, provided you follow the rules stated in the LGPL. You
can also modify this package; if you distribute a modified version,
you must distribute it under the terms of the LGPL, which in
particular means that you must release the source code for the
modified software. See
@link["http://www.gnu.org/licenses/lgpl-3.0.txt"]{http://www.gnu.org/licenses/lgpl-3.0.txt}
for more information.

@section{API}

@defmodule[net/rfc6455]

The interface is based on Racket's built-in
@racketmodname[net/websocket] API, with some extensions and
differences.

@defproc[(ws-conn? [x any/c]) boolean?]{

Returns @racket[#t] if and only if @racket[x] is a WebSocket
connection structure. Note that the structure defined by this package
is different from @racket[builtin:ws-conn?].

}

@defproc[(ws-conn-closed? [c ws-conn?]) boolean?]{

Returns @racket[#t] if the given connection has been closed, and
@racket[#f] otherwise.

}
 
@defproc[(ws-serve [conn-dispatch (-> ws-conn? request? void)]
		   [#:conn-headers conn-headers
				   (or/c (-> bytes? (listof header?) request?
					     (values (listof header?) any/c))
					 (-> bytes? (listof header?)
					     (values (listof header?) any/c)))]
		   ...)
	 (-> void)]{

This is a convenience entry point, largely directly compatible with
@racket[builtin:ws-serve], including all its arguments besides those
shown. If @racket[#:conn-headers] is supplied, then it is inspected to
see whether it takes two or three arguments, and is called
appropriately.}

@defproc[(ws-serve* [service-mapper (-> url? (-> (or/c symbol? #f) (or/c #f
									 (-> ws-conn? void))))]
		    ...)
	 (-> void)]

Like @racket[ws-serve], except uses the given @racket[service-mapper]
to decide how to handle an incoming request. See
@racket[ws-service-mapper].

@defform[(ws-service-mapper [uri-regexp [(protocol ...) function-expr] ...] ...)
	 #:grammar ([protocol symbol #f])]{

Macro that expands to an expression suitable for use as a
service-mapper with @racket[ws-serve*].

Each @racket[uri-regexp] is matched against an incoming request's URL
in turn until one matches. Then,

@itemize[

  @item{if the client supplied a @tt{Sec-WebSocket-Protocol} header,
each token from that header is checked against the @racket[protocol]s
in turn. If one matches, the corresponding @racket[function-expr] is
used as the connection handler; or,
}

  @item{if no such header was supplied, the first
@racket[function-expr] with a literal @racket[#f] among its
@racket[protocol]s is used.}

]

The @racket[function-expr]s must evaluate to connection handler
procedures, each taking a @racket[ws-conn?] as their only argument.

}

@defproc[(ws-send! [c ws-conn?]
		   [payload (or/c string? bytes? input-port?)]
		   [#:final-fragment? final-fragment? boolean? #t]
		   [#:payload-type payload-type (or/c 'continuation 'text 'binary) 'text]
		   [#:flush? flush? boolean? #t])
	 void?]{

Sends a message to the remote peer.

If @racket[payload] is a string, it is converted to bytes using
@racket[string->bytes/utf-8] before transmission. If it is an
input-port, it is read from and streamed using multiple WebSockets
message fragments to the peer until it yields @racket[eof] (see also
@racket[ws-stream-buffer-size]).

If @racket[flush?] is false, the buffers of the underlying connection
socket output-ports are not flushed after sending the message.
Otherwise (i.e. by default), they are flushed.

If @racket[payload-type] is @racket['text] or @racket['binary], the
appropriate WebSockets content type bit is set upon transmission.

Fragmented messages can be sent using this procedure.

@itemize[

  @item{The first fragment in a sequence must have
@racket[payload-type] set to @racket['text] or @racket['binary]. Every
subsequent fragment in the same sequence must have
@racket[payload-type] set to @racket['continuation].}

  @item{The final fragment in a sequence must have
@racket[final-fragment?] set to a non-false value. Every other
fragment in a sequence must have @racket[final-fragment?] set to
@racket[#f].}

]

For single-fragment (unfragmented) messages, the defaults are fine: a
plain @racket[(ws-send! c payload)] is enough. Here is an example of a
multi-fragment message:

@racketblock[(ws-send! c #"first" #:final-fragment? #f)
	     (ws-send! c #"second" #:final-fragment? #f #:payload-type 'continuation)
	     (ws-send! c #"third" #:final-fragment? #t #:payload-type 'continuation)]

}

@defproc[(ws-recv [c ws-conn?]
		  [#:stream? stream? boolean? #f]
		  [#:payload-type payload-type (or/c 'auto 'text 'binary) 'auto])
	 (or/c eof-object? string? bytes? input-port?)]{

Receives a message from the remote peer.

If @racket[stream?] is true, returns an input port from which the
bytes or characters making up the message can be read. An end-of-file
from the resulting input port is ambiguous: it does not separate the
end of the message being read from the end of the connection itself.
Use @racket[ws-conn?] to disambiguate.

If @racket[stream?] is @racket[#f], returns either a string or a
bytes, depending on @racket[payload-type]. If a specific
@racket['text] or @racket['binary] payload type is requested, the
corresponding result type is returned, or if @racket['auto] (the
default) is requested, the message's own text/binary indicator bit is
used to decide which to return.

Multi-fragment messages are transparently reassembled: in the case of
a returned input-port, fragment boundaries are not preserved, and in
the case of a returned string or bytes, the entire reassembled message
is returned.

}

@defproc[(ws-close! [c ws-conn?]
		    [#:status status integer? 1000]
		    [#:reason reason string? ""])
	 void?]{

Closes a connection. Has no effect if the connection is already
closed. The status code and reason are supplied to the remote peer in
the close frame.

}

@defparam[ws-stream-buffer-size size integer?]{

Used when streaming the contents of an input-port given to
@racket[ws-send!]. Streamed message fragments will be no larger than
@racket[(ws-stream-buffer-size)] bytes each.

}
