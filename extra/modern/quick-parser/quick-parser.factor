! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs bootstrap.syntax classes.parser
classes.tuple combinators combinators.short-circuit
combinators.smart fry generalizations hashtables io
io.encodings.utf8 io.files io.streams.string kernel lexer locals
macros make math modern.paths multiline namespaces prettyprint
sequences sequences.deep sequences.extras
sequences.generalizations sets shuffle strings
unicode.categories vectors vocabs.loader words ;
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

SYMBOL: string-literals
string-literals [ H{ } clone ] initialize

SYMBOL: literals
literals [ H{ } clone ] initialize

SYMBOL: paren-literals
paren-literals [ H{ } clone ] initialize

: ?push-at ( parser key assoc -- )
    [ ?push members ] change-at ;

: register-parser ( parser key -- ) qparsers get ?push-at ;

: register-paren-literal ( parser -- ) dup name>> paren-literals get ?push-at ;
: register-string-literal ( parser -- ) dup name>> string-literals get ?push-at ;
: register-literal ( parser -- ) dup name>> literals get ?push-at ;

ERROR: ambiguous-or-missing-parser seq ;
: choose-parser ( key assoc -- word/* )
    ?at
    [ dup length 1 = [ first ] [ ambiguous-or-missing-parser ] if ]
    [ drop f ] if ;

: lookup-action ( token -- word/f )
    dup slice? [ >string ] when
    qparsers get choose-parser ;


TUPLE: qsequence object slice ;
TUPLE: literal < qsequence ;
TUPLE: paren-literal < literal ;

TUPLE: compile-time-literal < literal ;
TUPLE: run-time-literal < literal ;

TUPLE: string-literal < literal ;
TUPLE: compile-time-long-string-literal < string-literal ;
TUPLE: run-time-long-string-literal < string-literal ;

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

! If it's whitespace or comment char, don't include it in the slice
:: take-until-either ( n string tokens -- slice/f n' string ch )
    n string '[ tokens member? ] find-from
    dup "!\s\r\n" member? [
        :> ( n' ch )
        n n' string ?<slice>
        n' string ch
    ] [
        [ dup [ 1 + ] when ] dip  :> ( n' ch )
        n n' string ?<slice>
        n' string ch
    ] if ; inline

ERROR: subseq-expected-but-got-eof n string expected ;
:: multiline-string-until' ( n string multi -- inside end n' string )
    multi string n start* :> n'
    n' [ n string multi subseq-expected-but-got-eof ] unless
    n n' string ?<slice>
    n' dup multi length + string ?<slice>
    n' multi length +  string ;

: multiline-string-until ( n string multi -- inside n' string )
    multiline-string-until' [ drop ] 2dip ; inline

: take-next ( n string -- ch n string )
    [ ?nth ]
    [ [ 1 + ] dip ] 2bi ;

: skip-blank ( n string -- n' string )
    [ [ blank? not ] find-from drop ] keep ; inline

: skip-til-eol ( n string -- n' string )
    [ [ "\r\n" member? ] find-from drop ] keep ; inline

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
DEFER: parse-until'
DEFER: parse-until
: tag-lexed ( opening object ending class -- literal )
    new
    [
        [ drop nip ] [
            nip
            [ from>> ]
            [ [ to>> ] [ seq>> ] bi ] bi* <slice>
        ] 3bi
    ] dip
        swap >>slice
        swap >>object ; inline

ERROR: unknown-string-literal opening seq ending ;
ERROR: unknown-literal opening seq ending ;
ERROR: unknown-paren-literal opening seq ending ;
ERROR: unknown-long-string-literal opening seq ending ;

! XXX: make sure no conflicts
: lookup-string ( obj -- class/f ) [ CHAR: " = ] trim-tail string-literals get choose-parser ;
: lookup-long-string ( obj -- class/f ) [ "[{=" member? ] trim-tail string-literals get choose-parser ;
: lookup-literal ( obj -- class/f ) [ "{[(" member? ] trim-tail literals get choose-parser ;
: lookup-paren-literal ( obj -- class/f ) [ CHAR: ( = ] trim-tail paren-literals get choose-parser ;

: make-string ( opening contents ending -- seq/literal )
    pick lookup-string [
        string-literal tag-lexed
    ] [
        string-literal tag-lexed
        ! unknown-string-literal
    ] if ; inline

: make-paren-literal ( opening contents ending -- seq/literal )
    pick lookup-paren-literal [
        paren-literal tag-lexed
    ] [
        paren-literal tag-lexed
        ! unknown-paren-literal
    ] if ; inline

: make-compile-time-literal ( opening contents ending -- seq/literal )
    pick lookup-literal [
        compile-time-literal tag-lexed
    ] [
        compile-time-literal tag-lexed
        ! unknown-literal
    ] if ; inline

: make-run-time-literal ( opening contents ending -- seq/literal )
    pick lookup-literal [
        run-time-literal tag-lexed
    ] [
        run-time-literal tag-lexed
        ! unknown-literal
    ] if ;

: make-compile-time-long-string ( opening contents ending -- seq/literal )
    pick lookup-long-string [
        compile-time-long-string-literal tag-lexed
    ] [
        compile-time-long-string-literal tag-lexed
        ! unknown-long-string-literal
    ] if ;

: make-run-time-long-string ( opening contents ending -- seq/literal )
    pick lookup-long-string [
        run-time-long-string-literal tag-lexed
    ] [
        run-time-long-string-literal tag-lexed
        ! unknown-long-string-literal
    ] if ;

ERROR: closing-paren-expected last n string ;
: read-paren ( seq n string -- seq n' string )
    2dup ?nth [ closing-paren-expected ] unless*
    blank? [
        ")" parse-until' [ make-paren-literal ] 2dip
    ] [
        complete-token
    ] if ;

: extend-slice ( slice n -- slice' )
    [ [ from>> ] [ to>> ] [ seq>> ] tri ] dip
    swap [ + ] dip <slice> ;

: slices>slice ( slices -- slice )
    [ first from>> ]
    [ last [ to>> ] [ seq>> ] bi ] bi <slice> ; inline

! [ ]
ERROR: long-bracket-opening-mismatch tok n string ch ;
:: read-long-bracket ( tok n string ch -- seq n string )
    ch {
        { CHAR: = [
            n string "[" take-including-separator :> ( tok2 n' string' ch )
                ch CHAR: [ = [ tok n string ch long-bracket-opening-mismatch ]  unless
        tok2 length 1 - CHAR: = <string> "]" "]" surround :> needle

            n' string' needle multiline-string-until' :> ( inside end n'' string'' )
            tok tok2 length extend-slice  inside  end make-run-time-long-string n'' string

        ] }
        { CHAR: [ [
            n 1 + string "]]" multiline-string-until' :> ( inside end n' string' )
            tok 1 extend-slice  inside  end make-run-time-long-string n' string
        ] }
        [ [ tok n string ] dip long-bracket-opening-mismatch ]
    } case ;

! { }
ERROR: long-brace-opening-mismatch tok n string ch ;
:: read-long-brace ( tok n string ch -- seq n string )
    ch {
        { CHAR: = [
            n string "{" take-including-separator :> ( tok2 n' string' ch )
                ch CHAR: { = [ tok n string ch long-brace-opening-mismatch ]  unless
            tok2 length 1 - CHAR: = <string> "}" "}" surround :> needle

            n' string' needle multiline-string-until' :> ( inside end n'' string'' )
            tok tok2 length extend-slice  inside  end make-compile-time-long-string  n'' string
        ] }
        { CHAR: { [
            n 1 + string "}}" multiline-string-until' :> ( inside end n' string' )
            tok 1 extend-slice  inside  end make-compile-time-long-string n' string
        ] }
    } case ;


ERROR: closing-bracket-expected last n string ;
: read-bracket ( last n string -- seq n' string )
    2dup ?nth [ closing-bracket-expected ] unless* {
        { [ dup "=[" member? ] [ read-long-bracket ] } ! double bracket, read [==[foo]==]
        { [ dup blank? ] [ drop "]" parse-until' [ make-run-time-literal ] 2dip ] } ! regular[ word
        [ drop complete-token ] ! something like [foo]
    } cond ;

ERROR: closing-brace-expected n string last ;
: read-brace ( n string seq -- n' string seq )
    2dup ?nth [ closing-brace-expected ] unless* {
        { [ dup "={" member? ] [ read-long-brace ] } ! double brace read {=={foo}==}
        { [ dup blank? ] [ drop "}" parse-until' [ make-compile-time-literal ] 2dip ] } ! regular{ word
        [ drop complete-token ] ! something like {foo}
    } cond ;

ERROR: string-expected-got-eof n string ;
: read-string' ( n string -- n' string )
    over [
        { CHAR: \ CHAR: " } take-including-separator {
            { f [ [ drop ] 2dip ] }
            { CHAR: " [ [ drop ] 2dip ] }
            { CHAR: \ [ take-next [ 2drop ] 2dip read-string' ] }
        } case
    ] [
        string-expected-got-eof
    ] if ;

:: read-string ( name n string -- seq n' string )
    n string read-string' :> ( n' seq' )
    name
    n dup 1 + string <slice>
    n' [ 1 - n' ] [ string length [ 2 - ] [ 1 - ] bi ] if* string <slice>
    make-string
    n' string ;

: advance-1 ( n string -- n/f string )
    over [
        [ 1 + ] dip 2dup length >= [ [ drop f ] dip ] when
    ] when ;

: head?-from ( seq n begin -- ? )
    [ tail-slice ] dip head? ; inline

DEFER: token
: take-comment ( tok n string -- n' string )
    [ 1 + ] dip
    2dup ?nth CHAR: [ = [
        [ 1 + ] dip 2dup ?nth read-long-bracket
        [ drop ] 2dip
    ] [
        [ drop ] 2dip skip-til-eol
    ] if ;

ERROR: whitespace-expected-after-string n string ch ;
: token ( n/f string -- token n'/f string )
    over [
        skip-blank over
        [
            ! XXX: check bad escape sequences
            ! seq n string ch
            "!([{\"\s\r\n" take-until-either {
                { f [ [ drop f ] dip ] } ! XXX: what here
                { CHAR: ! [ pick { [ empty? ] [ "#" sequence= ] } 1|| [ take-comment token ] [ complete-token ] if ] }
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

! XXX: simplify
ERROR: expected-error got expected ;
: expect ( n/f string obj -- n'/f string )
    -rot
    token
    [ 2dup sequence= [ nip ] [ expected-error ] if drop ] 2dip ;

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

ERROR: token-expected-but-got-eof n string expected ;
: parse-until' ( n string token -- obj/f last n/f string )
    pick [
        [ f ] 3dip 3dup
        '[
            [
                parse rot
                [
                    [ ] [ object>sequence ] bi _ sequence= not
                    [ [ , ] [ [ drop ] 3dip -rot ] if ] keep
                ] [ _ _ _ token-expected-but-got-eof ] if*
            ] loop
        ] { } make -roll
    ] [
        token-expected-but-got-eof
    ] if ;

: parse-until ( n string token -- obj n/f string )
    parse-until' [ drop ] 2dip ; inline

! XXX: simplify
: raw-until ( n/f string token -- obj/f n/f string )
    pick [
        [ f ] 3dip 3dup
        '[
            [
                raw rot
                [
                    [ ] [ object>sequence ] bi _ sequence= not
                    [ [ , ] [ [ drop ] 3dip -rot ] if ] keep
                ] [ _ _ _ token-expected-but-got-eof ] if*
            ] loop
        ] { } make -roll [ drop ] 2dip
    ] [
        token-expected-but-got-eof
    ] if ;

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



! Writing
GENERIC: write-parsed ( obj -- )
M: qsequence write-parsed slice>> write ;
M: slice write-parsed >string write ;
M: sequence write-parsed [ write-parsed ] each ;

: write-parsed-objects ( seq -- )
    [ write-parsed ] each nl ;

: write-modern-string ( seq -- string )
    [ write-parsed-objects ] with-string-writer ;

: write-modern-file ( seq path -- )
    utf8 [ write-parsed-objects ] with-file-writer ;




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

: define-string-literal ( name -- )
    [ string-literal { } define-tuple-class ] [ register-string-literal ] bi ;

: define-paren-literal ( name -- )
    [ paren-literal { } define-tuple-class ] [ register-paren-literal ] bi ;

: define-literal ( name -- )
    [ literal { } define-tuple-class ] [ register-literal ] bi ;

SYNTAX: STRING-LITERAL:
    scan-new-class define-string-literal ;

SYNTAX: PAREN-LITERAL:
    scan-new-class define-paren-literal ;

SYNTAX: LITERAL:
    scan-new-class define-literal ;
