! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays combinators constructors destructors fry
io io.private io.streams.position io.streams.string kernel
locals math math.order math.vectors namespaces sequences
sequences.private strings ;
IN: io.streams.document

TUPLE: pos { line integer } { column integer } ;
CONSTRUCTOR: <pos> pos ( line column -- pos ) ;

TUPLE: document-stream < position-stream
    { line integer } { column integer }
    { diff maybe{ pos } }
    { last-finish maybe{ pos } } ;

: <document-stream> ( stream -- document-stream )
    \ document-stream new-disposable
        swap >>stream
        f >>diff ; inline

: add-lines ( stream n -- stream ) '[ _ + ] change-line ; inline
: count-newlines ( string -- n ) [ CHAR: \n = ] count ;
: find-last-newline ( string -- n/f ) [ CHAR: \n = ] find-last drop ;
: count-trailing ( string -- n ) [ length ] [ find-last-newline ] bi [ - ] when* ;

TUPLE: doc { start pos } object { finish pos } ;
CONSTRUCTOR: <doc> doc ( start object finish -- doc ) ;

M: doc length object>> length ;
M: doc nth object>> nth ;
M: doc nth-unsafe object>> nth ;
M: doc integer>fixnum object>> integer>fixnum ;

GENERIC: calculate-finish-position ( start obj -- finish )

M: integer calculate-finish-position ( start obj -- finish )
    CHAR: \n = [
        line>> 0 <pos>
    ] [
        [ line>> ] [ column>> 1 + ] bi <pos>
    ] if ;

M: string calculate-finish-position ( start string -- finish )
    [ count-newlines ]
    [ length ]
    [ find-last-newline ] tri
    [ - [ line>> + ] dip <pos> ] [
        [ [ line>> ] [ column>> ] bi ] 2dip
        swapd + [ + ] dip <pos>
    ] if* ;

: doc-after ( doc object -- doc' )
    [ finish>> ] dip 2dup calculate-finish-position <doc> ;

: advance-string ( string stream -- )
    [
        [ count-newlines ] dip over 0 >
        [ swap add-lines 0 >>column drop ] [ 2drop ] if
    ] [
        swap
        [ length ]
        [ find-last-newline ] bi [
            - 1 - >>column drop
        ] [
            swap [ + ] change-column drop
        ] if*
    ] 2bi ;

: advance-1 ( stream n -- )
    CHAR: \n =
    [ 0 >>column 1 add-lines drop ]
    [ [ 1 + ] change-column drop ] if ; inline

: advance-stream-line ( stream -- ) 1 add-lines 0 >>column drop ; inline

M: document-stream stream-element-type call-next-method ;

M: document-stream stream-read
    [ nip stream-tell ] [ call-next-method ] [ nip stream-tell ] 2tri <doc> ;

M: document-stream stream-contents*
    [ stream-tell ] [ call-next-method ] [ stream-tell ] tri <doc> ;

M: document-stream stream-readln
    [ stream-tell ] [ call-next-method ] [ ] tri
    2dup advance-string
    [ stream-tell ] [ advance-stream-line ] bi <doc> ;

M: document-stream stream-read1
    [ stream-tell ] [ call-next-method ] [ ] tri
    [ over advance-1 ] [ stream-tell ] bi <doc> ;

M: document-stream stream-read-unsafe
    [ call-next-method ] 3keep
    rot drop pick 0 > [ advance-string ] [ 2drop ] if ;

M:: document-stream stream-read-until ( seps stream -- seq sep/f )
    stream stream-tell :> start-pos
    seps stream call-next-method :> ( read-seq read-sep )

    read-seq stream advance-string
    stream stream-tell :> seq-finish-pos
    read-seq [
        start-pos read-seq seq-finish-pos <doc>
    ] [
        f
    ] if
    stream read-sep advance-1
    stream stream-tell :> sep-finish-pos
    read-sep [
        seq-finish-pos read-sep sep-finish-pos <doc>
    ] [
        f
    ] if ;

M: document-stream stream-tell
    [ line>> ] [ column>> ] bi <pos> ;

: write-newlines ( pos stream -- )
    [ line>> CHAR: \n <string> ] [ stream>> ] bi* stream-write ;

: write-spaces ( pos stream -- )
    [ column>> CHAR: \s <string> ] [ stream>> ] bi* stream-write ;

: write-object ( doc stream -- )
    [ object>> ] [ stream>> ] bi*
    over integer? [ stream-write1 ] [ stream-write ] if ;

: docpos- ( start finish -- docpos )
    2dup [ line>> ] bi@ = [
        [ 0 ] 2dip [ column>> ] bi@ - <pos>
    ] [
        [ [ line>> ] bi@ - ] [ drop column>> ] 2bi <pos>
    ] if ;

: write-diff-spacing ( position stream -- )
    [ write-newlines ] [ write-spaces ] 2bi ;

: save-finish ( doc stream -- )
    [ finish>> ] dip last-finish<< ;

! Writing
M: document-stream stream-write ( doc stream -- )
    {
        [
            [ [ start>> ] [ last-finish>> ] bi* [ docpos- ] when* ] keep
            write-diff-spacing
        ]
        [ write-object ]
        [ [ object>> ] dip over integer? [ swap advance-1 ] [ advance-string ] if ]
        [ save-finish ]
    } 2cleave ;

M: document-stream stream-nl ( stream -- )
    stream>> stream-nl ;

: input>document-stream ( -- )
    input-stream [ <document-stream> ] change ;

: input-position ( -- n ) input-stream get n>> ;

: output>document-stream ( -- )
    output-stream [ <document-stream> ] change ;
