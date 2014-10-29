! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays ascii assocs combinators
combinators.short-circuit constructors fry io io.encodings.utf8
io.files io.streams.document io.streams.string kernel locals
make math math.parser namespaces sequences sequences.extras
strings ;
IN: modern.parser

! "TUPLE: foo a b c ;" parse-source-string
! "resource:core/math/math.factor" parse-modern-file

SYMBOL: parsers
parsers [ H{ } clone ] initialize

SYMBOL: comment-parsers
comment-parsers [ H{ } clone ] initialize

SYMBOL: comments
: save-comment ( comment -- )
    [ comments get push ] when* ;

TUPLE: parsed texts ;

TUPLE: mnumber < parsed n ;
CONSTRUCTOR: mnumber ( n -- mnumber ) ;

TUPLE: mstring < parsed class string ;
CONSTRUCTOR: mstring ( class string -- mstring ) ;

TUPLE: comment < parsed text ;
CONSTRUCTOR: comment ( text -- comment ) ;

TUPLE: nested-comment < parsed comment ;
CONSTRUCTOR: nested-comment ( comment -- nested-comment ) ;

TUPLE: typed-argument < parsed name signature ;
CONSTRUCTOR: typed-argument ( name signature -- typed ) ;

TUPLE: parser < parsed name slots syntax-name body ;
CONSTRUCTOR: parser ( name slots syntax-name body -- obj ) ;

TUPLE: literal-parser < parsed name ;
CONSTRUCTOR: literal-parser ( name -- obj ) ;

SYMBOL: current-texts
: save-current-texts ( text -- )
    current-texts get push ;

: with-texts ( quot -- )
    [ V{ } clone current-texts ] dip with-variable ; inline

: transfer-texts ( obj -- obj )
    current-texts get >>texts
    V{ } clone current-texts set ;

: texts-read-until ( seps -- seq sep )
    dup read-until over object>> "" = [
        2drop texts-read-until
    ] [
        over object>> [
            [ nip [ save-current-texts ] [ object>> ] bi ] dip
        ] [
            3drop f f
        ] if
    ] if ;

ERROR: string-expected got separator ;
: parse-string' ( -- )
    "\\\"" texts-read-until {
        { CHAR: " [ % ] }
        { CHAR: \ [ % read1 , parse-string' ] }
        { f [ f string-expected ] }
        [ string-expected ]
    } case ;

: parse-string ( class -- mstring )
    [ parse-string' ] "" make <mstring> ;

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

: token-loop ( -- string/f )
    "\r\n\s\"" texts-read-until {
        { [ dup "\r\n\s" member? ] [ drop [ token-loop ] when-empty ] }
        { [ 2dup [ empty? ] [ CHAR: " = ] bi* and ] [ drop [ f ] when-empty parse-string ] }
        { [ dup CHAR: " = ] [ drop [ f ] when-empty parse-string ] }
        ! { [ dup CHAR: # = ] [
            ! drop parse-comment save-comment [ token-loop ] when-empty ] }
        ! { [ dup CHAR: ! = ] [
            ! drop parse-comment save-comment [ token-loop ] when-empty ] }
        [ drop ]
    } cond ;

: token ( -- object )
    token-loop dup string? [
        dup string>number [ <mnumber> ] when
    ] when ;

: raw ( -- object )
    "\r\n\s" texts-read-until {
        { [ dup "\r\n\s" member? ] [ drop [ raw ] when-empty ] }
        [ drop ]
    } cond ;

: get-string ( -- string/f )
    "\r\n\s#" texts-read-until {
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
    [
        token parse-action dup { [ string? not ] [ ] } 1&& [ transfer-texts ] when
    ] with-texts ;

: parse-input ( -- seq comments )
    V{ } clone comments [
        [ parse ] loop>array
        comments get
    ] with-variable ;

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
    "\r\n" texts-read-until drop ;

ERROR: expected expected got ;
: expect ( string -- )
    token 2dup = [ 2drop ] [ expected ] if ;

: expect-one ( strings -- )
    token 2dup swap member? [ 2drop ] [ expected ] if ;

: body ( -- strings ) ";" parse-until ;

: parse-nested-comment ( -- nested-comment )
    "*)" parse-comment-until <nested-comment> ;

: parse-parser ( -- obj )
    token parse token ";" parse-until <parser> ;

: parse-literal-parser ( -- obj )
    token <literal-parser> ;

: register-parser ( parser key -- )
    parsers get-global set-at ;

\ parse-parser "PARSER:" register-parser
\ parse-literal-parser "LITERAL-PARSER:" register-parser

: parse-metadata ( path -- data )
    utf8 file-contents ;

: parse-stream ( stream -- seq comments )
    [ parse-input ] with-input-stream ; inline

: parse-source-file ( path -- data )
    utf8 [ input>document-stream parse-input ] with-file-reader drop ; inline

: parse-source-string ( string -- data )
    [ input>document-stream parse-input ] with-string-reader drop ; inline

ERROR: unrecognized-factor-file path ;
: parse-modern-file ( path -- seq )
    dup >lower {
        { [ dup ".txt" tail? ] [ drop dup parse-metadata 2array ] }
        { [ dup ".factor" tail? ] [ drop dup parse-source-file 2array ] }
        ! [ unrecognized-factor-file ]
        [ drop f 2array ]
    } cond ;

: write-parsed ( seq -- )
    [
        texts>> [ object>> write bl ] each nl
    ] each ;