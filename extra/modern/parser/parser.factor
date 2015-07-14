! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators
combinators.short-circuit constructors fry io io.encodings.utf8
io.files io.streams.document io.streams.string kernel make
math.parser namespaces prettyprint sequences sequences.extras
strings unicode.case vocabs.files vocabs.loader ;
IN: modern.parser

SYMBOL: parsers
parsers [ H{ } clone ] initialize

: register-parser ( parser key -- )
    parsers get-global set-at ;

TUPLE: parsed texts ;

DEFER: texts-readln
TUPLE: comment < parsed text ;
CONSTRUCTOR: <comment> comment ( text -- comment ) ;
: parse-comment ( -- comment ) texts-readln <comment> ;
\ parse-comment "!" register-parser
\ parse-comment "#!" register-parser

TUPLE: mnumber < parsed n ;
CONSTRUCTOR: <mnumber> mnumber ( n -- number ) ;

TUPLE: mstring < parsed class string ;
CONSTRUCTOR: <mstring> mstring ( class string -- string ) ;

TUPLE: mtoken < parsed name ;
CONSTRUCTOR: <mtoken> mtoken ( name -- token ) ;

TUPLE: new-class < parsed name ;
CONSTRUCTOR: <new-class> new-class ( name -- class ) ;

TUPLE: new-word < parsed name ;
CONSTRUCTOR: <new-word> new-word ( name -- word ) ;


SYMBOL: current-texts
: save-text ( text -- )
    dup object>> { "" CHAR: \s CHAR: \r CHAR: \n } member? [
        drop
    ] [
        current-texts get push
    ] if ;

! Call read-until first because sep is a string in the saved texts, we want a char
: texts-read-until ( seps -- seq sep )
    read-until
    dup [
        [ [ save-text ] [ object>> ] bi ] bi@
    ] [
        [
            dup [
                [ save-text ] [ object>> ] bi
            ] when
        ] dip
    ] if ;

: texts-read1 ( -- obj )
    read1 [ save-text ] [ object>> ] bi ;

: texts-readln ( -- string )
    readln [ save-text ] [ object>> ] bi ;

ERROR: string-expected got separator ;
: parse-string' ( -- )
    "\\\"" texts-read-until {
        { CHAR: " [ % ] }
        { CHAR: \ [ % texts-read1 , parse-string' ] }
        { f [ f string-expected ] }
        [ string-expected ]
    } case ;

: parse-string ( class -- mstring )
    [ parse-string' ] "" make <mstring> ;

: building-tail? ( string -- ? )
    [ building get ] dip {
        [ [ ?last ] bi@ = ]
        [ [ but-last-slice ] bi@ tail? ]
    } 2&& ;

ERROR: expected-sequence expected got ;
: multiline-string-until' ( seq -- )
    dup ?last 1array texts-read-until [
        [ % ] [ , ] bi*
        dup building-tail? [
            drop
        ] [
            multiline-string-until'
        ] if
    ] [
        % dup building-tail? [
            drop
        ] [
            building get >string expected-sequence
        ] if
    ] if* ;


: multiline-string-until ( end -- string )
    [ [ multiline-string-until' ] "" make ] keep length head* ;

ERROR: multiline-string-expected got ;
! multi"==[Lol. This string syntax...]=="
: parse-multiline-string ( class -- mstring )
    "[" texts-read-until [
        "]" "\"" surround multiline-string-until <mstring>
    ] [
        multiline-string-expected
    ] if ;

: execute-parser ( word -- object/f )
    dup name>> \ parsers get ?at [ execute( -- parsed ) nip ] [ drop ] if ;

: parse-action ( string -- object/f )
    dup mtoken? [
        dup name>> empty?
        [ drop f ] [ execute-parser ] if
    ] when ;

: token-loop ( -- string/f )
    "\r\n\s\"" texts-read-until {
        { [ dup "\r\n\s" member? ] [ drop [ token-loop ] when-empty ] }
        { [ dup CHAR: " = ] [
            drop f like
            dup "m" = [ parse-multiline-string ] [ parse-string ] if
        ] }
        [ drop ]
    } cond ;

ERROR: identifier-can't-be-number n ;

: identifer ( -- object )
    token-loop dup string? [
        dup string>number [
            identifier-can't-be-number
        ] [
            <mtoken>
        ] if
    ] when ;

: new-class ( -- object ) token-loop <new-class> ;
: new-word ( -- object ) token-loop <new-word> ;

: token ( -- object )
    token-loop dup string? [
        dup string>number [ <mnumber> ] [ <mtoken> ] if
    ] when ;

: raw ( -- object )
    "\r\n\s" texts-read-until {
        { [ dup "\r\n\s" member? ] [ drop [ raw ] when-empty ] }
        [ drop ]
    } cond ;

: strings-until ( string -- strings )
    '[
        _ raw 2dup = [ 2drop f ] [ nip ] if
    ] loop>array ;

ERROR: no-more-tokens ;
: parse ( -- object/f )
    token parse-action ;

ERROR: token-expected token ;
: parse-until ( string -- strings/f )
    '[
        _ parse [ token-expected ] unless*
        2dup dup mtoken? [ name>> ] when = [ 2drop f ] [ nip parse-action ] if
    ] loop>array ;

: string-until-eol ( -- string )
    "\r\n" texts-read-until drop ;

ERROR: expected expected got ;
: expect ( string -- )
    token
    2dup dup [ name>> ] when = [ 2drop ] [ expected ] if ;

: expect-one ( strings -- )
    token 2dup dup [ name>> ] when swap member? [ 2drop ] [ expected ] if ;

: body ( -- strings ) ";" parse-until ;

: parse-metadata ( path -- data ) utf8 file-contents ;

: with-texts ( quot -- )
    [ V{ } clone current-texts ] dip with-variable ; inline

: transfer-texts ( obj -- obj )
    current-texts get >>texts
    V{ } clone current-texts set ;

: parse-input-stream ( -- seq )
    [
        [ parse dup [ transfer-texts ] when ] loop>array
    ] with-texts ;

: parse-stream ( stream -- seq )
    [ parse-input-stream ] with-input-stream ; inline

: parse-source-file ( path -- data )
    utf8 [ input>document-stream parse-input-stream ] with-file-reader ; inline

: parse-modern-string ( string -- data )
    [ input>document-stream parse-input-stream ] with-string-reader ; inline

ERROR: unrecognized-factor-file path ;
: parse-modern-file ( path -- seq )
    dup >lower {
        { [ dup ".txt" tail? ] [ drop parse-metadata ] }
        { [ dup ".factor" tail? ] [ drop parse-source-file ] }
        { [ dup ".modern" tail? ] [ drop parse-source-file ] }
        [ unrecognized-factor-file ]
        ! [ drop f 2array ]
    } cond ;

: write-parsed-flat ( seq -- )
    [ texts>> [ object>> write bl ] each nl ] each ;

: write-parsed-objects ( seq -- )
    output>document-stream
    [ texts>> [ write ] each ] each nl ;

: write-parsed-string ( seq -- string )
    [ write-parsed-objects ] with-string-writer ;

: write-modern-file ( seq path -- )
    utf8 [ write-parsed-objects ] with-file-writer ;

: load-vocab-docs ( names -- seq )
    [ vocab-docs-path ] map
    [ exists? ] filter
    [ parse-modern-file ] map ;

: load-vocab-tests ( names -- seq )
    [ vocab-tests-path ] map
    [ exists? ] filter
    [ dup . flush parse-modern-file ] map ;
