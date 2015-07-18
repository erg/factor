! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs bootstrap.syntax classes.parser
classes.tuple combinators combinators.short-circuit
combinators.smart constructors fry io io.encodings.utf8 io.files
io.streams.document io.streams.string kernel lexer make math
math.parser namespaces parser prettyprint sequences
sequences.extras strings unicode.case vocabs.files vocabs.loader
words ;
IN: modern.parser

SYMBOL: parsers
parsers [ H{ } clone ] initialize
: register-parser ( parser key -- ) parsers get-global set-at ;
: clear-parsers ( -- ) parsers get-global clear-assoc ;

! Base classes
TUPLE: parsed object start finish ;
TUPLE: psequence object ;

: pbecome ( doc parser -- parser' )
    new
        over start>> >>start
        over finish>> >>finish
        swap object>> >>object ; inline

! These ARE parsed or psequences
TUPLE: ptext < parsed ;
TUPLE: ptoken < parsed ;
TUPLE: pnumber < parsed ;
TUPLE: pstring < psequence ;
TUPLE: pidentifier < parsed ;
TUPLE: pnew-class < parsed name ;
TUPLE: pexisting-class < parsed name ;
TUPLE: pnew-word < parsed name ;
TUPLE: pexisting-word < parsed name ;

: new-parsed ( type texts -- obj' ) [ new ] dip >>object ; inline

ERROR: string-expected got separator ;
: parse-string' ( -- )
    "\\\"" read-until object>> {
        { CHAR: " [ % ] }
        { CHAR: \ [ % read1 , parse-string' ] }
        { f [ f string-expected ] }
        [ string-expected ]
    } case ;

: parse-string ( class -- mstring )
    ! [ parse-string' ] "" make <pstring> ;
B
    [ parse-string' ] "" make 2array pstring swap new-parsed ;

: building-tail? ( string -- ? )
    [ building get ] dip {
        [ [ ?last ] bi@ = ]
        [ [ but-last-slice ] bi@ tail? ]
    } 2&& ;

ERROR: expected got expected ;
ERROR: expected-sequence expected got ;
: multiline-string-until' ( seq -- )
    dup ?last 1array read-until [
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
    "[" read-until [
        "]" "\"" surround multiline-string-until 2array pstring swap new-parsed
        ! "]" "\"" surround multiline-string-until <pstring>
    ] [
        multiline-string-expected
    ] if ;

: execute-parser ( word -- object/f )
    dup object>> \ parsers get ?at [ execute( -- parsed ) nip ] [ drop ] if ;

: parse-action ( string -- object )
    dup object>> \ parsers get ?at [
        execute( -- parsed ) [ swap ptext pbecome prefix ] change-object
    ] [ drop ] if ;

: token-loop' ( -- string/f )
    "\r\n\s\"" read-until {
        { [ dup f = ] [ drop ] } ! must be above object>>
        { [ dup object>> "\r\n\s" member? ] [ drop [ token-loop' ] when-empty ] }
        { [ dup object>> CHAR: " = ] [
B
            drop f like
            dup "m" = [ parse-multiline-string ] [ parse-string ] if
        ] }
        ! [ dup . flush B drop ]
    } cond ;

: token-loop ( type -- token/f )
    [ token-loop' ] dip pbecome ; ! [ new-parsed ] [ drop f ] if* ;

: raw ( -- object )
    "\r\n\s" read-until {
        { [ dup f = ] [ drop ] }
        { [ dup object>> "\r\n\s" member? ] [ drop [ raw ] when-empty ] }
    } cond ;

: cut-last ( seq -- before last )
    dup length 1 - cut first ;

ERROR: token-expected token ;
: raw-until ( string -- strings sep )
    dup '[
        raw [ dup object>> _ = [ ptext pbecome , f ] when ] [ _ token-expected ] if*
    ] loop>array cut-last ;

! ERROR: identifier-can't-be-number n ;

: new-identifier ( -- object ) pidentifier token-loop ;
: new-class ( -- object ) pnew-class token-loop ;
: existing-class ( -- object ) pexisting-class token-loop ;
: new-word ( -- object ) pnew-word token-loop ;
: existing-word ( -- object ) pexisting-word token-loop ;
: token ( -- object ) token-loop' ;
: parse ( -- object/f ) token-loop' dup [ parse-action ] when ;

! XXX: parsing word named ";" will execute while parse-until is looking for a ; -- may never find it!
! XXX: fix is to call token here and parse-action manually
: parse-until ( string -- strings sep )
    dup '[
        parse [ _ token-expected ] unless*
        dup object>> _ = [ , f ] when
        ! dup doc? [ ptoken pbecome ] when
    ] loop>array cut-last ptext pbecome ;

: expect ( string -- string )
    token
    2dup dup [ object>> ] when = [ nip ptext pbecome ] [ expected ] if ;

: body ( -- strings last ) ";" parse-until ;

: parse-metadata ( path -- data ) utf8 file-contents ;

: parse-input-stream ( -- seq ) [ parse ] loop>array ;

: parse-stream ( stream -- seq )
    [ parse-input-stream ] with-input-stream ; inline

: parse-source-file ( path -- data )
    utf8 [ input>document-stream parse-input-stream ] with-file-reader ; inline

: parse-modern-string ( string -- data )
    [ input>document-stream parse-input-stream ] with-string-reader ; inline

: parse-vocab ( string -- data )
    modern-source-path parse-source-file ;

ERROR: unrecognized-factor-file path ;
: parse-modern-file ( path -- seq )
    dup >lower {
        { [ dup ".txt" tail? ] [ drop parse-metadata ] }
        { [ dup ".factor" tail? ] [ drop parse-source-file ] }
        { [ dup ".modern" tail? ] [ drop parse-source-file ] }
        [ unrecognized-factor-file ]
        ! [ drop f 2array ]
    } cond ;

GENERIC: write-parsed ( obj -- )
M: parsed write-parsed object>> write ;
M: psequence write-parsed object>> [ write-parsed ] each ;

GENERIC: write-pflat' ( obj -- )
M: parsed write-pflat' object>> write bl ;
M: psequence write-pflat' object>> [ write-parsed ] each ;

: write-pflat ( seq -- )
    [ write-pflat ] each nl ;

: write-pobjects ( seq -- )
    output>document-stream
    [ write-parsed ] each nl ;

: write-pstring ( seq -- string )
    [ write-pobjects ] with-string-writer ;

: write-modern-file ( seq path -- )
    utf8 [ write-pobjects ] with-file-writer ;

: load-vocab-docs ( names -- seq )
    [ vocab-docs-path ] map
    [ exists? ] filter
    [ parse-modern-file ] map ;

: load-vocab-tests ( names -- seq )
    [ vocab-tests-path ] map
    [ exists? ] filter
    [ dup . flush parse-modern-file ] map ;

<<
: define-parser ( class token quot -- )
    [ 2drop psequence { } define-tuple-class ]
    [
        [ [ name>> "parse-" prepend create-word-in mark-top-level-syntax ] keep ] 2dip
        {
            [ nip swap '[ _ output>array _ boa ] ( -- obj ) define-declared ]
            [ drop nip register-parser ]
        } 4cleave
    ] 3bi ;
>>

SYNTAX: PARSER:
    scan-new-class
    scan-token
    parse-definition define-parser ;
