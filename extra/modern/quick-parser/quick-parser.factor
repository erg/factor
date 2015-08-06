! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs bootstrap.syntax classes.parser
classes.tuple combinators combinators.smart fry generalizations
io.encodings.utf8 io.files kernel lexer locals macros make math
modern.paths multiline namespaces sequences sequences.extras
sequences.generalizations sets strings unicode.categories
vectors words shuffle ;
QUALIFIED: parser
QUALIFIED: lexer
FROM: assocs => change-at ;
IN: modern.quick-parser

/*
"resource:basis/regexp/regexp-tests.factor" quick-parse-path

[ dup . flush yield quick-parse-path drop ] each
USE: modern.quick-parser
"math" quick-parse-vocab
[ >string . ] each
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


TUPLE: parsed-atom { slice slice } ;
TUPLE: parsed-compound atoms { slice slice } ;

TUPLE: qsequence object slice ;

: ?<slice> ( n n' string -- slice )
    over [ nip [ length ] keep ] unless <slice> ; inline

! Include the separator, which is not whitespace
:: take-until-separator ( n string tokens -- slice/f n' string ch/f )
    n string '[ tokens member? ] find-from [ dup [ 1 + ] when ] dip  :> ( n' ch )
    n n' [ string length ] unless* string <slice>
    n' string ch ; inline

:: take-no-separator ( n string tokens -- slice/f n' string ch/f )
    n string '[ tokens member? ] find-from :> ( n' ch )
    n n' [ string length ] unless* string <slice>
    n' string ch ; inline

! Don't include the whitespace
:: take-until-whitespace ( n string -- slice/f n' string ch/f )
    n string '[ "\s\r\n" member? ] find-from :> ( n' ch )
    n n' [ string length ] unless* string <slice>
    n' string ch ; inline

! If it's whitespace, don't include it
:: take-until-either ( n string tokens -- slice/f n' string ch )
    n string '[ tokens member? ] find-from dup "\s\r\n" member? [
        :> ( n' ch )
        n n' [ string length ] unless* string <slice>
        n' string ch
    ] [
        [ dup [ 1 + ] when ] dip  :> ( n' ch )
        n n' [ string length ] unless* string <slice>
        n' string ch
    ] if ; inline

:: take-until-multi ( n string multi -- inside end/f n' string )
    multi string n start* :> n'
    n n' string <slice>
    n' dup multi length + string <slice>
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
        ")" parse-until -roll [ prefix ] 2dip
    ] [
        complete-token
    ] if ;

: extend-slice ( slice n -- slice' )
    [ [ from>> ] [ to>> ] [ seq>> ] tri ] dip
    swap [ + ] dip <slice> ;

! Ugly
:: read-long-bracket ( tok n string ch -- seq n string )
    ch {
        { CHAR: = [
            n string "[" take-until-separator CHAR: [ = [ "omg error" throw ] unless :> ( tok2 n' string' )
            tok2 length 1 - CHAR: = <string> "]" "]" surround :> needle

            n' string' needle take-until-multi :> ( inside end n'' string'' )
            tok tok2 length extend-slice
            inside
            end 3array
            n''
            string
        ] }
        { CHAR: [ [
            n 1 + string "]]" take-until-multi :> ( inside end n' string' )
            tok 1 extend-slice
            inside
            end 3array
            n'
            string
        ] }
    } case ;

:: read-long-brace ( tok n string ch -- seq n string )
    ch {
        { CHAR: = [
            n string "{" take-until-separator CHAR: { = [ "omg error" throw ] unless :> ( tok2 n' string' )
            tok2 length 1 - CHAR: = <string> "}" "}" surround :> needle

            n' string' needle take-until-multi :> ( inside end n'' string'' )
            tok tok2 length extend-slice
            inside
            end 3array
            n''
            string
        ] }
        { CHAR: { [
            n 1 + string "}}" take-until-multi :> ( inside end n' string' )
            tok 1 extend-slice
            inside
            end 3array
            n'
            string
        ] }
    } case ;


ERROR: closing-bracket-expected last n string ;
: read-bracket ( last n string -- seq n' string )
    2dup ?nth [ closing-bracket-expected ] unless* {
        { [ dup "=[" member? ] [ read-long-bracket ] } ! double bracket, read [==[foo]==]
        { [ dup blank? ] [ drop "]" parse-until -roll [ prefix ] 2dip ] } ! regular[ word
        [ drop complete-token ] ! something like [foo]
    } cond ;

ERROR: closing-brace-expected n string last ;
: read-brace ( n string seq -- n' string seq )
    2dup ?nth [ closing-brace-expected ] unless* {
        { [ dup "={" member? ] [ read-long-brace ] } ! double brace read {=={foo}==}
        { [ dup blank? ] [ drop "}" parse-until -roll [ prefix ] 2dip ] } ! regular{ word
        [ drop complete-token ] ! something like {foo}
    } cond ;

: read-string' ( n string -- n' string )
    "\"\\" take-until-separator {
        { f [ "error123" throw ] }
        { CHAR: " [ [ drop ] 2dip ] }
        { CHAR: \ [ drop read-string' ] }
        [ "errorr1212" throw ]
    } case ;

:: read-string ( name n string -- seq n' string )

    n string read-string' :> ( n' seq' )
    name
    n n' 1 - string <slice>
    n' 1 - n' string <slice>
    3array
    n'
    string ;

: token ( n/f string -- token n'/f string )
    over [
        skip-blank over
        [
            "!([{\"\s\r\n" take-until-either
            ! n string seq ch
            {
                { f [ ] } ! XXX: what here
                { CHAR: ! [ drop skip-til-eol token ] }
                { CHAR: ( [ read-paren ] }
                { CHAR: [ [ read-bracket ] }
                { CHAR: { [ read-brace ] }
                { CHAR: " [ read-string ] }
                [ drop ] ! "\s\r\n" found
            } case
            ! ensure-token [ drop token ] [ nip ] if
        ] [ 2drop f f f ] if
    ] [
        2drop f f f
    ] if ; inline recursive

: raw ( n/f string -- slice/f n'/f string )
    over [
        skip-blank take-until-whitespace [ drop ] 2dip
    ] [
        2drop f f f
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
        ! tok [ tok n' string parse-action ] [ tok n' string ] if
    ] [
        f f f
    ] if ;
    ! over
    ! [ token pick [ parse-action ] when ] ! prefix here
    ! [ 2drop f f f ] if ; inline

: parse-until ( n/f string token -- obj/f n/f string )
    '[ [ parse rot [ dup , _ sequence= not ] [ f ] if* ] loop ] { } make ;

: qparse ( string -- sequence )
    [ 0 ] dip [ parse rot ] loop>array 2nip ;

: quick-parse-path ( path -- sequence )
    utf8 file-contents qparse ;

: quick-parse-vocab ( path -- sequence )
    modern-source-path quick-parse-path ;

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
            ! qparse-foo
            ! (word) class token quot
            [
                [ drop ] 3dip
                [ '[ _ expect ] ] dip compose
                swap
                '[ _ 2 output>array-n [ swap ] 2dip [ ?<slice> _ boa ] 2keep ]
                ( n n' string -- obj n' string ) define-declared
            ] [ ! (qparse-foo)
                [ drop ] 4dip nip swap
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
