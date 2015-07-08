! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays combinators constructors destructors fry
io io.private io.streams.position io.streams.string kernel
locals math math.order math.vectors namespaces sequences
sequences.private strings ;
IN: io.streams.document

TUPLE: document-position { line integer } { column integer } ;
CONSTRUCTOR: <document-position> document-position ( line column -- document-position ) ;

TUPLE: document-stream < position-stream
    { line integer } { column integer }
    { diff maybe{ document-position } }
    { last-finish maybe{ document-position } } ;

: <document-stream> ( stream -- document-stream )
    \ document-stream new-disposable
        swap >>stream
        f >>diff ; inline

: add-lines ( stream n -- stream ) '[ _ + ] change-line ; inline
: advance-stream-line ( stream -- ) 1 add-lines 0 >>column drop ; inline
: count-newlines ( string -- n ) [ CHAR: \n = ] count ;
: find-last-newline ( string -- n/f ) [ CHAR: \n = ] find-last drop ;
: count-trailing ( string -- n ) [ length ] [ find-last-newline ] bi [ - ] when* ;
: add-pairs ( a x b y -- ax by ) [ swap [ + ] dip ] dip + ; inline
GENERIC: object-length ( obj -- nl trailing )
M: integer object-length CHAR: \n = [ 1 0 ] [ 0 1 ] if ;
M: sequence object-length [ count-newlines ] [ count-trailing ] bi ;
: docpos- ( docpos1 docpos2 -- docpos )
    [ [ line>> ] bi@ - ]
    [ [ column>> ] bi@ - ] 2bi <document-position> ;

: docpos+ ( docpos1 docpos2 -- docpos )
    [ [ line>> ] bi@ + ]
    [ [ column>> ] bi@ + ] 2bi <document-position> ;

: object-diff ( object -- position )
    [ object>> object-length <document-position> ]
    [ length>> ] bi docpos- ;

TUPLE: document-object object { start document-position } { finish document-position } ; ! { length document-position } ;
CONSTRUCTOR: <document-object> document-object ( start object finish -- document-object ) ;
    ! dup object>> object-length <document-position> >>length ;

M: document-object length object>> length ;
M: document-object nth object>> nth ;
M: document-object nth-unsafe object>> nth ;
M: document-object integer>fixnum object>> integer>fixnum ;

! GENERIC: calculate-finish-position ( start obj -- finish )

! M: integer calculate-finish-position ( start obj -- finish )
    ! CHAR: \n = [
        ! line>> 0 <document-position>
    ! ] [
        ! [ line>> ] [ column>> 1 + ] bi <document-position>
    ! ] if ;

! M: string calculate-finish-position ( start string -- finish )
    ! [ count-newlines ]
    ! [ length ]
    ! [ find-last-newline ] tri
    ! [ - [ line>> + ] dip <document-position> ] [
        ! [ [ line>> ] [ column>> ] bi ] 2dip
        ! swapd + [ + ] dip <document-position>
    ! ] if* ;

! : document-object-after ( document-object object -- document-object' )
    ! [ finish>> ] dip 2dup calculate-finish-position <document-object> ;

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

M: document-stream stream-element-type call-next-method ;

M: document-stream stream-read
    [ nip stream-tell ] [ call-next-method ] [ nip stream-tell ] 2tri <document-object> ;

M: document-stream stream-contents*
    [ stream-tell ] [ call-next-method ] [ stream-tell ] tri <document-object> ;

M: document-stream stream-readln
    [ stream-tell ] [ call-next-method ] [ ] tri
    [ advance-stream-line ] [ stream-tell ] bi <document-object> ;

M: document-stream stream-read1
    [ stream-tell ] [ call-next-method ] [ ] tri
    [ over advance-1 ] [ stream-tell ] bi <document-object> ;

M: document-stream stream-read-unsafe
    [ call-next-method ] 3keep
    rot drop pick 0 > [ advance-string ] [ 2drop ] if ;

M:: document-stream stream-read-until ( seps stream -- seq sep/f )
    stream stream-tell :> start-pos
    seps stream call-next-method :> ( read-seq read-sep )

    read-seq stream advance-string
    stream stream-tell :> seq-finish-pos
    read-seq [
        start-pos read-seq seq-finish-pos <document-object>
    ] [
        f
    ] if
    stream read-sep advance-1
    stream stream-tell :> sep-finish-pos
    read-sep [
        seq-finish-pos read-sep sep-finish-pos <document-object>
    ] [
        f
    ] if ;

M: document-stream stream-tell
    [ line>> ] [ column>> ] bi <document-position> ;

: write-newlines2 ( document-position stream -- )
    [ [ line>> ] bi@ [-] CHAR: \n <string> ]
    [ nip [ dup length ] dip swap dup 0 > [ add-lines 0 >>column drop ] [ 2drop ] if ]
    [ nip stream>> ] 2tri stream-write ;

: write-spaces2 ( document-position stream -- )
    [ [ column>> ] bi@ [-] CHAR: \s <string> ]
    [ nip [ dup length ] dip swap dup 0 > [ '[ _ + ] change-column drop ] [ 2drop ] if ]
    [ nip stream>> ] 2tri stream-write ;

: write-newlines ( document-position stream -- )
    [ line>> CHAR: \n <string> ] [ stream>> ] bi* stream-write ;

: write-spaces ( document-position stream -- )
    [ column>> CHAR: \s <string> ] [ stream>> ] bi* stream-write ;

: write-object ( document-object stream -- )
    [ object>> ] [ stream>> ] bi*
    over integer? [ stream-write1 ] [ stream-write ] if ;

: write-offset ( offset stream -- )
    [
        over line>> 0 >
        [ [ line>> CHAR: \n <string> ] dip stream>> stream-write ] [ 2drop ] if
    ] [
        over column>> 0 >
        [ [ column>> CHAR: \s <string> ] dip stream>> stream-write ] [ 2drop ] if
    ] bi-curry bi ;

: calculate-start-diff ( document-object stream -- position )
    [ [ start>> ] [ diff>> ] bi* [ docpos+ ] when* ]
    [ nip last-finish>> [ swap docpos- ] when* ] 2bi ;

: docpos2+ ( start finish -- docpos )
    2dup [ line>> ] bi@ = [
        [ 0 ] 2dip [ column>> ] bi@ - abs <document-position>
    ] [
        drop
    ] if ;

: calculate-start ( document-object stream -- position )
    [ start>> ] [ last-finish>> ] bi* [ docpos2+ ] when* ;

: goto-start ( position stream -- )
    [ write-newlines ] [ write-spaces ] 2bi ;

: calculate-diff ( document-object stream -- )
    [ [ object-diff ] [ diff>> ] bi* [ docpos+ ] when* ] keep diff<< ;

: save-finish ( document-object stream -- )
    [ finish>> ] dip last-finish<< ;

! Writing
M: document-stream stream-write ( document-object stream -- )
    {
        ! Go to the: start + diff
        [ [ calculate-start ] keep goto-start ]
        ! Write the object
        [ write-object ]
        ! Advance stream
        [ [ object>> ] dip over integer? [ swap advance-1 ] [ advance-string ] if ]
        ! Calculate the new diff - newline resets columns
        ! [ calculate-diff ]
        ! Save finish
        [ save-finish ]
    } 2cleave ;

M: document-stream stream-nl ( stream -- )
    stream>> stream-nl ;

: input>document-stream ( -- )
    input-stream [ <document-stream> ] change ;

: input-position ( -- n ) input-stream get n>> ;

: output>document-stream ( -- )
    output-stream [ <document-stream> ] change ;

: documents>string ( documents -- string )
    [
        output>document-stream
        [ texts>> [ write ] each ] each nl
    ] with-string-writer ;
