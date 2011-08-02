! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors destructors io kernel math sequences splitting
fry strings combinators ;
IN: io.streams.document

! Readers
TUPLE: document-stream < disposable stream line# column# previous-character ;
TUPLE: document-reader < document-stream ;
TUPLE: document-writer < document-stream ;

TUPLE: @token text offset line# column# ;

: <@token> ( stream string -- token )
    swap
    [ @token new ] 2dip
        [ >>text ] dip
        [ line#>> >>line# ]
        [ column#>> >>column# ] bi
        tell-input >>offset ; inline

: tell/string>token ( tell string -- token )
    @token new
        swap >>text
        swap [ line#>> >>line# ] [ column#>> >>column# ] bi ; inline
        
: next-line ( stream -- )
    [ 1 + ] change-line#
    0 >>column# drop ;

: update-line1 ( stream character -- )
    >>previous-character next-line ;

: update-column ( stream character -- )
    >>previous-character [ 1 + ] change-column# drop ;

: update-line-read ( stream string -- )
    string-lines
    [ length 1 - dup 0 > [ '[ _ + ] change-line# 0 >>column# drop ] [ 2drop ] if ]
    [ last length '[ _ + ] change-column# drop ] 2bi ;

: update-stream1 ( stream character -- )
    over previous-character>> CHAR: \r = [
        update-line1
    ] [
        dup CHAR: \n = [
            update-line1
        ] [
            update-column
        ] if
    ] if ;

: update-stream ( stream string -- )
    over previous-character>> CHAR: \r = [
        [ last update-line1 ]
        [ update-line-read ] 2bi
    ] [
        update-line-read
    ] if ;

M: document-reader stream-element-type drop +character+ ;

M: document-reader stream-read1
    dup stream>> stream-read1 [
        [ <@token> ]
        [ update-stream1 ]
        2bi
    ] [
        drop f
    ] if* ;

M: document-reader stream-peek1
    dup stream>> stream-peek1
    [ <@token> ] [ drop f ] if* ;

M: document-reader stream-read
    [ nip ] [ stream>> stream-read ] 2bi [
        [ <@token> ]
        [ update-stream ]
        2bi
    ] [
        drop f
    ] if* ;

M: document-reader stream-read-until
    [ nip ] [ stream>> stream-read-until ] 2bi
    over [
        pick previous-character>> CHAR: \r = [
            pick next-line
        ] when
        [
            [ nip >>previous-character drop ]
            [ drop <@token> ]
            [ drop update-line-read ] 3tri
        ] 3keep
        nip
        [
            <@token>
        ] [
            CHAR: \n = [ next-line ] [ [ 1 + ] change-column# drop ] if
        ] 2bi
    ] [
        3drop f f
    ] if ;

M: document-reader stream-peek
    [ nip ] [ stream>> stream-peek ] 2bi
    [ <@token> ] [ drop f ] if* ;

TUPLE: document-stream-marker offset line# column# ;

: <document-stream-marker> ( offset line# column# -- document-stream-marker )
    document-stream-marker new
        swap >>column#
        swap >>line#
        swap >>offset ; inline

M: document-reader stream-tell
    [ stream>> stream-tell ]
    [ line#>> ]
    [ column#>> ] tri <document-stream-marker> ;

ERROR: document-stream-seek-absolute-only seek-type stream ;

: check-seek-type ( seek-type stream -- seek-type stream )
    over seek-absolute = [ document-stream-seek-absolute-only ] unless ;

M: document-reader stream-seek
    check-seek-type
    [ [ offset>> ] 2dip stream>> stream-seek ]
    [
        nip
        [ [ line#>> ] dip line#<< ]
        [ [ column#>> ] dip column#<< ] 2bi
    ] 3bi ;

ERROR: backwards-line-seek token stream line-to line-from ;
ERROR: backwards-column-seek token stream column-to column-from ;

: check-backwards-line-seek ( token stream -- token stream )
    2dup [ line#>> ] bi@ 2dup <
    [ backwards-line-seek ] [ 2drop ] if ;

: check-backwards-column-seek ( token stream -- token stream )
    2dup [ column#>> ] bi@ 2dup <
    [ backwards-column-seek ] [ 2drop ] if ;

: same-line? ( token stream -- ? )
    check-backwards-line-seek
    [ line#>> ] bi@ = ;

: seek-lines ( token stream -- )
    2dup same-line? [
        2drop
    ] [
        {
            [ [ line#>> ] bi@ - CHAR: \n <string> ]
            [ nip stream>> stream-write ]
            [ [ line#>> ] dip line#<< ]
            [ nip 0 >>column# drop ]
        } 2cleave
    ] if ;

: seek-columns ( token stream -- )
    check-backwards-column-seek
    [ [ column#>> ] bi@ - CHAR: \s <string> ]
    [ nip stream>> stream-write ]
    [ [ column#>> ] dip column#<< ] 2tri ;

: seek-forward ( token stream -- token stream )
    [ seek-lines ]
    [ seek-columns ]
    [ ] 2tri ;

M: document-stream dispose* stream>> dispose ;

: new-document-stream ( stream document-stream-class -- stream' )
    new-disposable
        swap >>stream
        0 >>line#
        0 >>column# ; inline

: <document-reader> ( stream -- stream' )
    document-reader new-document-stream ; inline

: <document-writer> ( stream -- stream' )
    document-writer new-document-stream ; inline

: document-stream-write ( string stream -- )
    [ stream>> stream-write ]
    [ swap update-stream ] 2bi ;

: document-stream-write1 ( string stream -- )
    [ stream>> stream-write1 ]
    [ swap update-stream1 ] 2bi ;

: seek-token ( token strema -- string stream )
    over @token? [ seek-forward [ text>> ] dip ] when ;

M: document-writer stream-write
    seek-token document-stream-write ;
    
M: document-writer stream-write1
    seek-token document-stream-write1 ;

M: document-writer stream-nl
    [ next-line ]
    [ stream>> stream-nl ] bi ;

: with-document-reader ( stream quot -- )
    [ <document-reader> ] dip with-input-stream ; inline
