! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays combinators constructors destructors io
io.streams.position kernel math namespaces sequences ;
IN: io.streams.document

TUPLE: document-stream < position-stream { line integer } { column integer } ;
: <document-stream> ( stream -- document-stream )
    \ document-stream new-disposable
        swap >>stream ; inline

TUPLE: document-position { line integer } { column integer } ;
CONSTRUCTOR: document-position ( line column -- document-position ) ;

TUPLE: document-object { position document-position } object ;
CONSTRUCTOR: document-object ( position object -- document-object ) ;

: with-advance-line ( stream quot -- seq )
    [ call ] 2keep drop
    [ 1 + ] change-line 0 >>column drop ; inline

: count-newlines ( string -- n )
    [ CHAR: \n = ] count ;

: find-last-newline ( string -- n ? )
    [ CHAR: \n = ] find-last >boolean ;

: advance-string ( string stream -- )
    [ [ count-newlines ] dip over 0 > [ [ + ] change-line 0 >>column drop ] [ 2drop ] if ]
    [
        swap
        [ length ] [ find-last-newline ] bi [
            - 1 - >>column drop
        ] [
            drop swap [ + ] change-column drop
        ] if
    ] 2bi ;

: advance-1 ( stream n -- )
    CHAR: \n =
    [ 0 >>column [ 1 + ] change-line drop ]
    [ [ 1 + ] change-column drop ] if ; inline

: with-advance-1 ( n stream quot -- n )
    [ call ] 2keep drop over object>> advance-1 ; inline

M: document-stream stream-element-type call-next-method ;

! stream-read-unsafe advances
M: document-stream stream-read
    [ nip stream-tell ] [ call-next-method ] 2bi <document-object> ;

M: document-stream stream-contents*
    [ stream-tell ] [ call-next-method ] bi <document-object> ;

M: document-stream stream-readln
    [
        [ stream-tell ] [ call-next-method ] bi <document-object>
    ] with-advance-line ;

M: document-stream stream-read1
    [
        [ stream-tell ] [ call-next-method ] bi <document-object>
    ] with-advance-1 ;

M: document-stream stream-read-unsafe
    [ call-next-method ] 3keep
    rot drop pick 0 > [ advance-string ] [ 2drop ] if ;

M: document-stream stream-read-until
    [ nip stream-tell ] ! pos
    [ call-next-method [ [ <document-object> ] keep ] dip ]
    [ nip ] 2tri ! seq sep stream
    {
        [ nip advance-string ]
        [ swap advance-1 drop ]
        [ drop nip ]
    } 3cleave ;

M: document-stream stream-tell
    [ line>> ] [ column>> ] bi <document-position> ;


: input>document-stream ( -- )
    input-stream [ <document-stream> ] change ;

: input-position ( -- n ) input-stream get n>> ;

: output>document-stream ( -- )
    output-stream [ <document-stream> ] change ;
