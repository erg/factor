! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators
combinators.short-circuit constructors fry io io.encodings.utf8
io.files io.streams.document io.streams.string kernel make
math.parser namespaces prettyprint sequences sequences.extras
strings unicode.case vocabs.files vocabs.loader ;
IN: modern.parser

! goal: remove this
SYMBOL: current-texts

: with-texts ( quot -- )
    [ V{ } clone current-texts ] dip with-variable ; inline

: push-text ( text -- )
    current-texts get push ;

: last-text ( -- last/f )
    current-texts get ?last ;

! : transfer-texts ( obj -- obj )
    ! current-texts get >>texts
    ! V{ } clone current-texts set ;

SYMBOL: parsers
parsers [ H{ } clone ] initialize

: register-parser ( parser key -- )
    parsers get-global set-at ;

! Testing
: clear-parsers ( -- ) parsers get-global clear-assoc ;

! Base classes
TUPLE: parsed text start finish ;
TUPLE: parsed-sequence texts ;

! These ARE parsed or parsed-sequences
TUPLE: parsed-token < parsed ;
TUPLE: parsed-number < parsed ;
TUPLE: parsed-string < parsed-sequence ;
TUPLE: parsed-identifier < parsed ;
TUPLE: parsed-new-class < parsed name ;
TUPLE: parsed-existing-class < parsed name ;
TUPLE: parsed-word < parsed name ;

: new-parsed ( type texts -- obj' ) [ new ] dip >>text ; inline

: text>object ( text -- obj )
    [
        dup object>> dup
        { "" CHAR: \s CHAR: \r CHAR: \n } member? [
            nip
        ] [
            [ push-text ] dip
        ] if
    ] [
        f
    ] if* ;

: texts-read-until ( seps -- seq sep ) read-until [ text>object ] bi@ ;
: texts-read1 ( -- obj ) read1 text>object ;
: texts-readln ( -- string ) readln text>object ;

ERROR: string-expected got separator ;
: parse-string' ( -- )
    "\\\"" texts-read-until {
        { CHAR: " [ % ] }
        { CHAR: \ [ % texts-read1 , parse-string' ] }
        { f [ f string-expected ] }
        [ string-expected ]
    } case ;

: parse-string ( class -- mstring )
    ! [ parse-string' ] "" make <parsed-string> ;
    [ parse-string' ] "" make 2array parsed-string swap new-parsed ;

: building-tail? ( string -- ? )
    [ building get ] dip {
        [ [ ?last ] bi@ = ]
        [ [ but-last-slice ] bi@ tail? ]
    } 2&& ;

ERROR: expected got expected ;
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
        "]" "\"" surround multiline-string-until 2array parsed-string swap new-parsed
        ! "]" "\"" surround multiline-string-until <parsed-string>
    ] [
        multiline-string-expected
    ] if ;

: execute-parser ( word -- object/f )
    dup text>> \ parsers get ?at [ execute( -- parsed ) nip ] [ drop ] if ;

: parse-action ( string -- object/string )
    \ parsers get ?at [ execute( -- parsed ) ] when ;
    ! dup parsed-token? [
        ! dup text>> empty?
        ! [ drop f ] [ execute-parser ] if
    ! ] when ;

: token-loop' ( -- string/f )
    "\r\n\s\"" texts-read-until {
        { [ dup "\r\n\s" member? ] [ drop [ token-loop' ] when-empty ] }
        ! { [ dup CHAR: " = ] [
            ! drop f like
            ! dup "m" = [ parse-multiline-string ] [ parse-string ] if
        ! ] }
        [ drop ]
    } cond ;

: token-loop ( type -- token/f )
    token-loop' [ new-parsed ] [ drop f ] if* ;

: raw ( -- object )
    "\r\n\s" texts-read-until {
        { [ dup "\r\n\s" member? ] [ drop [ raw ] when-empty ] }
        [ drop ]
    } cond ;

: raw-until ( string -- strings )
    '[
        _ raw 2dup = [ 2drop f ] [ nip ] if
    ] loop>array ;

! ERROR: identifier-can't-be-number n ;

: new-identifier ( -- object ) parsed-identifier token-loop ;
: new-class ( -- object ) parsed-new-class token-loop ;
: existing-class ( -- object ) parsed-existing-class token-loop ;
: new-word ( -- object ) parsed-word token-loop ;
: token ( -- object ) token-loop' ;
: parse ( -- object/f ) token-loop' parse-action ;

ERROR: token-expected token ;
! XXX: parsing word named ";" will execute while parse-until is looking for a ; -- may never find it!
: parse-until ( string -- strings/f )
    dup '[
        ! XXX: fix is to call token here and parse-action manually
        parse [ _ token-expected ] unless*
        dup _ = [ drop f ] when
    ] loop>array ;

: expect ( string -- )
    token
    2dup dup [ text>> ] when = [ 2drop ] [ expected ] if ;

: body ( -- strings ) ";" parse-until ;

: parse-metadata ( path -- data ) utf8 file-contents ;

: parse-input-stream ( -- seq )
    [
        ! [ parse dup [ transfer-texts ] when ] loop>array
        [ parse ] loop>array
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
