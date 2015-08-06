! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs bootstrap.syntax classes.parser
classes.tuple combinators combinators.smart fry generalizations
io.encodings.utf8 io.files kernel lexer locals macros make math
modern.paths multiline namespaces prettyprint sequences
sequences.deep sequences.extras sequences.generalizations sets
shuffle strings unicode.categories vectors vocabs.loader words ;
QUALIFIED: parser
QUALIFIED: lexer
FROM: assocs => change-at ;
IN: modern.quick-parser

/*
clear
0 "\"\\\"\""
"!([{\"\s\r\n" take-until-either 1string .
{ CHAR: \ CHAR: " } take-including-separator [ >string . ] 3dip 1string .
drop read-string'
*/


SYMBOL: qparsers

qparsers [ H{ } clone ] initialize

: register-parser ( parser key -- )
    qparsers get [ ?push members ] change-at ;

ERROR: ambiguous-or-missing-parser seq ;
: ensure-action ( seq -- word/* )
    dup length 1 = [ first ] [ ambiguous-or-missing-parser ] if ;

: lookup-action ( token -- word/f )
    dup slice? [ >string ] when
    qparsers get ?at [ ensure-action ] [ drop f ] if ;


TUPLE: qsequence object slice ;

: ?<slice> ( n n' string -- slice )
    over [ nip [ length ] keep ] unless <slice> ; inline

! Increment past the separator
:: take-including-separator ( n string tokens -- slice/f n' string ch/f )
    n string '[ tokens member? ] find-from [ dup [ 1 + ] when ] dip  :> ( n' ch )
    n n' string ?<slice>
    n' string ch ; inline

:: take-excluding-separator ( n string tokens -- slice/f n' string ch/f )
    n string '[ tokens member? ] find-from :> ( n' ch )
    n n' string ?<slice>
    n' string ch ; inline

! Don't include the whitespace in the slice
:: take-until-whitespace ( n string -- slice/f n' string ch/f )
    n string '[ "\s\r\n" member? ] find-from :> ( n' ch )
    n n' string ?<slice>
    n' string ch ; inline

! If it's whitespace, don't include it in the slice
:: take-until-either ( n string tokens -- slice/f n' string ch )
    n string '[ tokens member? ] find-from
    dup "\s\r\n" member? [
        :> ( n' ch )
        n n' string ?<slice>
        n' string ch
    ] [
        [ dup [ 1 + ] when ] dip  :> ( n' ch )
        n n' string ?<slice>
        n' string ch
    ] if ; inline

:: multiline-string-until ( n string multi -- inside end/f n' string )
    multi string n start* :> n'
    n n' string ?<slice>
    n' dup multi length + string ?<slice>
    n' multi length + string ;

: take-next ( n string -- ch n string )
    [ ?nth ]
    [ [ 1 + ] dip ] 2bi ;

: skip-blank ( n string -- n' string )
    [ [ blank? not ] find-from drop ] keep ; inline

: skip-til-eol ( n string -- n' string )
    [ [ "\r\n" member? ] find-from drop ] keep ; inline

: prepend-slice ( end begin -- slice )
    [ nip from>> ]
    [ drop [ to>> ] [ seq>> ] bi <slice> ] 2bi ; inline

: append-slice ( begin end -- slice )
    [ drop from>> ]
    [ nip [ to>> ] [ seq>> ] bi <slice> ] 2bi ; inline

: complete-token ( token n string -- token' n' string )
    take-until-whitespace drop [ append-slice ] 2dip ;

: parse-action ( token n/f string -- obj/f n string )
    pick lookup-action [
        [ from>> ] 3dip
        execute( n n' string -- obj/f n string )
    ] [ ] if* ;

DEFER: parse
DEFER: parse-until

ERROR: closing-paren-expected last n string ;
: read-paren ( seq n string -- seq n' string )
    2dup ?nth [ closing-paren-expected ] unless*
    blank? [
        ")" parse-until [ swap prefix ] 2dip
    ] [
        complete-token
    ] if ;

: extend-slice ( slice n -- slice' )
    [ [ from>> ] [ to>> ] [ seq>> ] tri ] dip
    swap [ + ] dip <slice> ;

! [ ]
:: read-long-bracket ( tok n string ch -- seq n string )
    ch {
        { CHAR: = [
            n string "[" take-including-separator CHAR: [ = [ "omg error" throw ] unless :> ( tok2 n' string' )
            tok2 length 1 - CHAR: = <string> "]" "]" surround :> needle

            n' string' needle multiline-string-until :> ( inside end n'' string'' )
            tok tok2 length extend-slice  inside  end 3array n'' string
        ] }
        { CHAR: [ [
            n 1 + string "]]" multiline-string-until :> ( inside end n' string' )
            tok 1 extend-slice  inside  end 3array n' string
        ] }
    } case ;

! { }
:: read-long-brace ( tok n string ch -- seq n string )
    ch {
        { CHAR: = [
            n string "{" take-including-separator CHAR: { = [ "omg error" throw ] unless :> ( tok2 n' string' )
            tok2 length 1 - CHAR: = <string> "}" "}" surround :> needle

            n' string' needle multiline-string-until :> ( inside end n'' string'' )
            tok tok2 length extend-slice  inside  end 3array n'' string
        ] }
        { CHAR: { [
            n 1 + string "}}" multiline-string-until :> ( inside end n' string' )
            tok 1 extend-slice  inside  end 3array n' string
        ] }
    } case ;


ERROR: closing-bracket-expected last n string ;
: read-bracket ( last n string -- seq n' string )
    2dup ?nth [ closing-bracket-expected ] unless* {
        { [ dup "=[" member? ] [ read-long-bracket ] } ! double bracket, read [==[foo]==]
        { [ dup blank? ] [ drop "]" parse-until [ swap prefix ] 2dip ] } ! regular[ word
        [ drop complete-token ] ! something like [foo]
    } cond ;

ERROR: closing-brace-expected n string last ;
: read-brace ( n string seq -- n' string seq )
    2dup ?nth [ closing-brace-expected ] unless* {
        { [ dup "={" member? ] [ read-long-brace ] } ! double brace read {=={foo}==}
        { [ dup blank? ] [ drop "}" parse-until [ swap prefix ] 2dip ] } ! regular{ word
        [ drop complete-token ] ! something like {foo}
    } cond ;

ERROR: string-expected-got-eof n string ;
: read-string' ( n string -- n' string )
    over [
        { CHAR: \ CHAR: " } take-including-separator {
        ! "\"\\" take-including-separator {
            { f [ [ drop ] 2dip ] }
            { CHAR: " [ [ drop ] 2dip ] }
            { CHAR: \ [ take-next [ 2drop ] 2dip read-string' ] }
            [ "impossible" throw ]
        } case
    ] [
        string-expected-got-eof
    ] if ;

:: read-string ( name n string -- seq n' string )
    n string read-string' :> ( n' seq' )
    name
    n dup 1 + string <slice>
    n' [ 1 - n' ] [ string length [ 2 - ] [ 1 - ] bi ] if* string <slice>
    3array n' string ;

: advance-1 ( n string -- n/f string )
    over [
        [ 1 + ] dip 2dup length >= [ [ drop f ] dip ] when
    ] when ;

ERROR: whitespace-expected-after-string n string ch ;
: token ( n/f string -- token n'/f string )
    over [
        skip-blank over
        [
            ! XXX: check bad escape sequences
            ! seq n string ch
            "!([{\"\s\r\n" take-until-either {
                { f [ [ drop f ] dip ] } ! XXX: what here
                { CHAR: ! [ pick empty? [ [ drop ] 2dip skip-til-eol token ] [ complete-token ] if ] }
                { CHAR: ( [ read-paren ] }
                { CHAR: [ [ read-bracket ] }
                { CHAR: { [ read-brace ] }
                { CHAR: " [ read-string take-next rot [ dup blank? [ drop ] [ whitespace-expected-after-string ] if ] when* ] }
                [ drop ] ! "\s\r\n" found
            } case
            ! ensure-token [ drop token ] [ nip ] if
        ] [ [ drop f f ] dip ] if
    ] [
        [ drop f f ] dip
    ] if ; inline recursive

: new-class ( n/f string -- token n'/f string ) token ;
: new-word ( n/f string -- token n'/f string ) token ;
: existing-class ( n/f string -- token n'/f string ) token ;
: existing-word ( n/f string -- token n'/f string ) token ;
: body ( n/f string -- seq n'/f string ) ";" parse-until ;

: raw ( n/f string -- slice/f n'/f string )
    over [
        skip-blank take-until-whitespace drop
    ] [
        [ drop f f ] dip
    ] if ;

ERROR: expected-error got expected ;
: expect ( n/f string obj -- obj n'/f string )
    -rot
    token
    [ 2dup sequence= [ nip ] [ expected-error ] if ] 2dip ;

:: parse ( n string -- obj/f n'/f string )
    n [
        n string token :> ( tok n' string )
        tok n' string tok [ parse-action ] when
    ] [
        f f string
    ] if ;

GENERIC: object>sequence ( obj -- string )

M: qsequence object>sequence slice>> ;
M: object object>sequence ;

ERROR: token-expected-but-got-eof n string token ;
: parse-until ( n/f string token -- obj/f n/f string )
    pick [
        dup '[
            [ parse rot [ [ , ] [ object>sequence ] bi _ sequence= not
        ] [
            _ token-expected-but-got-eof ] if* ] loop
        ] { } make -rot
    ] [
        token-expected-but-got-eof
    ] if ;

: raw-until ( n/f string token -- obj/f n/f string )
    '[ [ raw rot [ dup , _ sequence= not ] [ f ] if* ] loop ] { } make -rot ;

: qparse ( string -- sequence )
    [ 0 ] dip [ parse rot ] loop>array 2nip ;

: quick-parse-path ( path -- sequence )
    utf8 file-contents qparse ;

: quick-parse-vocab ( path -- sequence )
    modern-source-path quick-parse-path ;

: qparse-vocab ( path -- seq )
    vocab-source-path quick-parse-path ;

GENERIC: >out ( obj -- string )

M: slice >out >string ;
M: qsequence >out slice>> >out ;
M: sequence >out [ >out ] map ;
M: string >out ;

: qparse-vocab. ( path -- )
    qparse-vocab
    [ >out ] deep-map [ . ] each ;

<<
: define-qparser ( class token quot -- )
    [ 2drop qsequence { } define-tuple-class ]
    [
        [
            2drop name>> "qparse-" prepend
            [ parser:create-word-in mark-top-level-syntax ]
            [ "(" ")" surround parser:create-word-in ] bi
        ] 3keep
        ! word (word) class token quot
        {
            ! (word) class token quot
            [
                [ drop ] 3dip
                [ '[ _ expect ] ] dip compose
                swap
                '[ _ 2 output>array-n [ swap ] 2dip [ ?<slice> _ boa ] 2keep ]
                ( n n' string -- obj n' string ) define-declared
            ] [ ! (qparse-foo)
                [ drop ] 4dip nip
                [ 2dup 2drop ] prepose ! stack depth should be 2 for the macro below....
                swap
                '[ _ 2 output>array-n [ swap ] 2dip [ ?<slice> _ boa ] 2keep ]
                ( n n' string -- obj n' string ) define-declared
            ] [ ! (word) token register
                [ drop ] 4dip drop nip register-parser
            ]
        } 5 ncleave
    ] 3bi ;
>>

SYNTAX: QPARSER:
    scan-new-class
    scan-token
    parser:parse-definition define-qparser ;
