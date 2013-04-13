! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: arrays assocs combinators constructors formatting fry io
io.encodings.utf8 io.files io.streams.position kernel locals
make math math.parser namespaces sequences sequences.extras
strings unicode.case ;
IN: modern.parser

SYMBOL: parsers
parsers [ H{ } clone ] initialize

SYMBOL: comment-parsers
comment-parsers [ H{ } clone ] initialize

SYMBOL: comments
: save-comment ( comment -- )
    [ comments get push ] when* ;

TUPLE: mnumber n ;
CONSTRUCTOR: mnumber ( n -- mnumber ) ;

ERROR: string-expected got separator ;
TUPLE: mstring class string ;
CONSTRUCTOR: mstring ( class string -- mstring ) ;


SYMBOL: texts

TUPLE: text string from to ;
CONSTRUCTOR: text ( string from to -- text ) ;

: next-text ( quot -- text )
    input-position [ call ] dip input-position <text> ; inline

: save-text ( text -- )
    texts get push ;

: string-n>token ( string n -- token )
    2dup [ length ] dip + <text> ;

:: token-read-until ( sep -- string sep )
    input-position :> start
    sep read-until :> ( string sep' )
    string input-position 2 - string-n>token save-text
    sep' input-position [ 1 - ] keep <text> save-text
    string sep' ;

: parse-string' ( -- )
    "\\\"" read-until {
        { CHAR: " [ % ] }
        { CHAR: \ [ % read1 , parse-string' ] }
        { f [ f string-expected ] }
        [ string-expected ]
    } case ;

: parse-string ( class -- mstring )
    [ parse-string' ] "" make <mstring> ;

TUPLE: comment text ;
CONSTRUCTOR: comment ( text -- comment ) ;
: parse-comment ( -- comment ) readln <comment> ;

: execute-parser ( word -- object/f )
    \ parsers get ?at [ execute( -- parsed ) ] when ;

: parse-action ( string -- object/f )
    dup string? [
        [ f ] [ execute-parser ] if-empty
    ] when ;

: execute-comment-parser ( word -- object/f )
    \ comment-parsers get ?at [ execute( -- parsed ) ] when ;

: comment-parse-action ( string -- object/f )
    dup string? [
        [ f ] [ execute-comment-parser ] if-empty
    ] when ;

: token' ( -- string/f )
    "\r\n\s\"" read-until {
        { [ dup "\r\n\s" member? ] [ drop [ token' ] when-empty ] }
        { [ 2dup [ empty? ] [ CHAR: " = ] bi* and ] [ drop [ f ] when-empty parse-string ] }
        { [ dup CHAR: " = ] [ drop [ f ] when-empty parse-string ] }
        ! { [ dup CHAR: # = ] [
            ! drop parse-comment save-comment [ token' ] when-empty ] }
        ! { [ dup CHAR: ! = ] [
            ! drop parse-comment save-comment [ token' ] when-empty ] }
        [ drop ]
    } cond ;

: token ( -- object )
    token' dup string? [
        dup string>number [ <mnumber> ] when
    ] when ;

: raw ( -- object )
    "\r\n\s" read-until {
        { [ dup "\r\n\s" member? ] [ drop [ raw ] when-empty ] }
        [ drop ]
    } cond ;

: get-string ( -- string/f )
    "\r\n\s#" read-until {
        { [ dup "\r\n\s" member? ] [ drop [ get-string ] when-empty ] }
        { [ dup CHAR: # = ] [
            drop parse-comment save-comment [ get-string ] when-empty ] }
        [ drop ]
    } cond ;

: strings-until ( string -- strings )
    '[
        _ get-string 2dup = [ 2drop f ] [ nip ] if
    ] loop>array ;

ERROR: no-more-tokens ;
: parse ( -- object/f )
    token parse-action ;

: parse-input ( -- seq comments )
    V{ } clone comments [
        [ parse ] loop>array
        comments get
    ] with-variable ;

: parse-metadata ( path -- data )
    utf8 file-contents ;

: parse-source-file ( path -- data )
    utf8 [ input>position-stream parse-input ] with-file-reader drop ; inline

ERROR: unrecognized-factor-file path ;

! TODO: Save comments here
: parse-file ( path -- seq )
    dup >lower {
        { [ dup ".txt" tail? ] [ drop dup parse-metadata 2array ] }
        { [ dup ".factor" tail? ] [ drop dup parse-source-file 2array ] }
        ! [ unrecognized-factor-file ]
        [ drop f 2array ]
    } cond ;

: parse-stream ( stream -- seq comments )
    [ parse-input ] with-input-stream ; inline

ERROR: token-expected token ;
: parse-until ( string -- strings/f )
    '[
        _ token [ token-expected ] unless*
        2dup = [ 2drop f ] [ nip parse-action ] if
    ] loop>array ;

ERROR: raw-expected raw ;
: parse-comment-until ( string -- strings/f )
    '[
        _ raw [ raw-expected ] unless*
        2dup = [ 2drop f ] [ nip comment-parse-action ] if
    ] loop>array ;

: string-until-eol ( -- string )
    "\r\n" read-until drop ;

ERROR: expected expected got ;
: expect ( string -- )
    token 2dup = [ 2drop ] [ expected ] if ;

: expect-one ( strings -- )
    token 2dup swap member? [ 2drop ] [ expected ] if ;

: body ( -- strings ) ";" parse-until ;

TUPLE: nested-comment comment ;
CONSTRUCTOR: nested-comment ( comment -- nested-comment ) ;
: parse-nested-comment ( -- nested-comment )
    "*)" parse-comment-until <nested-comment> ;

TUPLE: signature in out ;
CONSTRUCTOR: signature ( in out -- signature ) ;

TUPLE: typed-argument name signature ;
CONSTRUCTOR: typed-argument ( name signature -- typed ) ;

DEFER: parse-signature(--)
DEFER: parse-nested-signature(--)
DEFER: parse-signature-in
DEFER: parse-signature-in'

: parse-signature-in'' ( -- )
    raw dup ":" tail? [
        parse-nested-signature(--) <typed-argument> ,
        parse-signature-in''
    ] [
        dup "--" = [
            drop
        ] [
            , parse-signature-in''
        ] if
    ] if ;

: parse-signature-in' ( -- out )
    [ parse-signature-in'' ] { } make ;

: parse-signature-in ( -- in )
    "(" expect parse-signature-in' ;

: parse-signature-out' ( -- )
    raw dup ":" tail? [
        parse-nested-signature(--) <typed-argument> ,
        parse-signature-out'
    ] [
        dup ")" = [
            drop
        ] [
            , parse-signature-out'
        ] if
    ] if ;

: parse-signature-out ( -- out )
    [ parse-signature-out' ] { } make ;

: parse-nested-signature(--) ( -- signature )
    raw dup "(" = [
        drop
        parse-signature-in' parse-signature-out <signature>
    ] when ;

: parse-signature(--) ( -- signature )
    parse-signature-in parse-signature-out <signature> ;

: parse-signature--) ( -- signature )
    parse-signature-in' parse-signature-out <signature> ;

TUPLE: parser name slots syntax-name body ;
CONSTRUCTOR: parser ( name slots syntax-name body -- obj ) ;
: parse-parser ( -- obj )
    token parse token ";" parse-until <parser> ;

TUPLE: literal-parser name ;
CONSTRUCTOR: literal-parser ( name -- obj ) ;
: parse-literal-parser ( -- obj )
    token <literal-parser> ;

: register-parser ( parser key -- )
    parsers get-global set-at ;

\ parse-parser "PARSER:" register-parser
\ parse-literal-parser "LITERAL-PARSER:" register-parser
