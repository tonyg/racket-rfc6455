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

This package, @tt{rfc6455}, provides
@link["http://tools.ietf.org/html/rfc6455"]{RFC 6455} compatible
WebSockets server and client interfaces for Racket, building on Racket's
@racket[web-server] collection.

Besides support for RFC 6455, the final WebSockets standard, the
package also incorporates code supporting the earlier, draft
@link["http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-00"]{hybi-00}
proposal, because several common older browsers still use this
protocol variant.

Wikipedia has a
@link["http://en.wikipedia.org/wiki/WebSocket#Browser_support"]{good
section on browser support for WebSocket protocol variations}, which
states "All the latest browsers except Android browser support the
latest specification (RFC 6455) of the WebSocket protocol."

This package has been developed against

@itemize[
  @item{Firefox 24.0 (which is an RFC 6455 peer)}
  @item{Chrome 30.0.1599.101 (which is an RFC 6455 peer)}
  @item{Safari 5.1.10 (which is a hybi-00 peer)}
]

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

Creating a client connection:

@racketblock[
  (require net/rfc6455)
  (require net/url)
  (define c (ws-connect (string->url "ws://localhost:8081/")))
  (ws-send! c "Hello world!")]

@section{License}

All the code in this package is licensed under the LGPL, version 3.0
or any later version. Each source file has a brief copyright and
licensing notice attached, but see
@link["http://www.gnu.org/licenses/lgpl-3.0.txt"]{the licence text
itself} for full details.

The only exceptions to the above are the files marked "public domain"
in the @tt{net/rfc6455/examples} directory. They are intended to be
examples of usage of the package for people to build on without
concern for licensing minutiae.

@section{API}

@defmodule[net/rfc6455]

The interface is based on Racket's built-in
@racketmodname[net/websocket] API, with some extensions and
differences.

@defproc[(ws-url? [x any/c]) boolean?]{

Returns true if and only if @racket[x] is a @racket[url?] and has a
@racket[url-scheme] equal to @racket["ws"] or @racket["wss"].

}

@defproc[(wss-url? [x any/c]) boolean?]{

Returns true if and only if @racket[x] is a @racket[url?] and has a
@racket[url-scheme] equal to @racket["wss"].

}

@defproc[(ws-conn? [x any/c]) boolean?]{

Returns @racket[#t] if and only if @racket[x] is a WebSocket
connection as defined by this package. Note that this is quite
different from @racket[builtin:ws-conn?].

}

@defproc[(ws-conn-supports-fragmentation? [c ws-conn?]) boolean?]{

@racket[#t] iff the connected peer supports sending and receiving
fragmented/streamed messages. Currently, RFC 6455-compliant peers
support fragmentation, while hybi-00 peers do not.

}

@defproc[(ws-conn-supports-payload-type? [c ws-conn?] [payload-type symbol?]) boolean?]{

@racket[#t] iff the connected peer supports the requested payload
type. RFC 6455-compliant peers support types @racket['text] and
@racket['binary], while hybi-00 peers support only @racket['text].

}

@defproc[(ws-conn-signals-status-on-close? [c ws-conn?]) boolean?]{

@racket[#t] iff the @racket[status] and/or @racket[reason] values are
communicated to the remote peer on connection
close (@racket[ws-close!]). RFC 6455 includes space in the wire
protocol for these values, while hybi-00 does not.

}

@defproc[(ws-conn-closed? [c ws-conn?]) boolean?]{

Returns @racket[#t] if the given connection has been closed, and
@racket[#f] otherwise.

}

@defproc[(ws-connect [u (or/c ws-url? wss-url?)]
		     [#:headers headers (listof header?) '()]
		     [#:protocol protocol (or/c 'rfc6455 'hybi00) 'rfc6455])
	 ws-conn?]{

Connect to the given WebSockets server, via TLS if @racket[(wss-url?
u)]. Supplies @racket[headers] as additional headers in the WebSocket
handshake request. A protocol variant other than the RFC 6455 standard
can be chosen by supplying a value for the @racket[#:protocol]
parameter.

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

(Note: Only RFC 6455 peers support fragmentation and non-text
payloads. Attempts to use these features with hybi-00 peers will
signal an error. See @racket[ws-conn-supports-fragmentation?] and
@racket[ws-conn-supports-payload-type?].)

If @racket[payload] is a string, it is converted to bytes using
@racket[string->bytes/utf-8] before transmission. If it is an
input-port, it is read from and streamed using multiple WebSockets
message fragments to the peer until it yields @racket[eof] (see also
@racket[rfc6455-stream-buffer-size]).

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

(Note: Only RFC 6455 peers support streaming and non-text payloads.
Attempts to use these features with hybi-00 peers will signal an
error. See @racket[ws-conn-supports-fragmentation?] and
@racket[ws-conn-supports-payload-type?].)

If @racket[stream?] is true, returns an input port from which the
bytes or characters making up the message can be read. An end-of-file
from the resulting input port is ambiguous: it does not separate the
end of the message being read from the end of the connection itself.
Use @racket[ws-conn-closed?] to disambiguate.

If @racket[stream?] is @racket[#f], returns either a string or a
bytes, depending on @racket[payload-type]. If a specific
@racket['text] or @racket['binary] payload type is requested, the
corresponding result type is returned, or if @racket['auto] (the
default) is requested, the message's own text/binary indicator bit is
used to decide which to return. If @racket[eof] occurs mid-message,
fragments so far received are discarded and @racket[eof] is returned.

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

(Note: hybi-00 peers do not have room in their wire protocol for the
status and reason codes. See
@racket[ws-conn-signals-status-on-close?].)

}

@defparam[rfc6455-stream-buffer-size size integer?]{

Used when streaming the contents of an input-port given to
@racket[ws-send!]. Streamed message fragments will be no larger than
@racket[(rfc6455-stream-buffer-size)] bytes each.

}

@defparam[hybi00-framing-mode mode (or/c 'new 'old)]{

Used with @emph{pre-}hybi-00 peers. Selects either the "new" or "old"
framing modes. You only have to worry about this if you're trying to
communicate with truly ancient, pre-hybi-00 peers, and then you no
doubt have bigger problems.

}

@defparam[ws-idle-timeout seconds number?]{

Idle timeout in seconds. If the interval between successive received
frames (of any type) exceeds this number of seconds, the connection
will be closed.

This parameter defaults to 300 seconds, i.e. five minutes.

}
