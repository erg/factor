! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs bootstrap.syntax classes.parser
classes.tuple combinators combinators.short-circuit
combinators.smart constructors fry generalizations io
io.encodings.utf8 io.files io.streams.document io.streams.string
kernel lexer locals make math math.parser modern.paths multiline
namespaces prettyprint sequences sequences.extras
sequences.generalizations shuffle splitting strings unicode.case
vocabs.files vocabs.loader words ;
QUALIFIED: parser
IN: modern.parser

SYMBOL: parsers
parsers [ H{ } clone ] initialize
: register-parser ( parser key -- ) parsers get-global set-at ;
: clear-parsers ( -- ) parsers get-global clear-assoc ;

! Base classes
TUPLE: psequence object ;

: psequence-become ( psequence psequence-class -- parser' )
    new
        swap object>> >>object ; inline

ERROR: parser-expected class pos ;
ERROR: token-expected expected pos ;
ERROR: expected expected got pos ;
ERROR: expected-sequence expected got pos ;
ERROR: double-quote-expected partial-string got pos ;
: doc-become ( doc doc-class -- parser' )
    over [ nip tell-input parser-expected ] unless
    new
        over start>> >>start
        over finish>> >>finish
        swap object>> >>object ; inline

! These ARE parsed or psequences
TUPLE: ptext < doc ;
TUPLE: pstring-text < doc ;
TUPLE: pnew-class < doc ;
TUPLE: pexisting-class < doc ;
TUPLE: pnew-word < doc ;
TUPLE: pexisting-word < doc ;

TUPLE: pstring < psequence ;
TUPLE: parguments < psequence ;
TUPLE: pbody < psequence ;
TUPLE: prun-time < psequence ;
TUPLE: pcompile-time < psequence ;
TUPLE: pcontainer < psequence ;

: building-tail? ( string -- ? )
    [ building get ] dip {
        [ [ ?last ] bi@ = ]
        [ [ but-last-slice ] bi@ tail? ]
    } 2&& ;

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
            building get >string tell-input expected-sequence
        ] if
    ] if* ;

! XXX: cleanup so ugly
: multiline-string-until ( end -- string sep )
    [ tell-input ] dip
    [ '[ _ multiline-string-until' ] "" make ] keep
    [ ?tail drop tell-input doc boa ] keep
    tell-input tuck doc boa
    dup object>> [ count-newlines ] [ count-trailing ] bi <rel>
    [ nip '[ _ pos-rel- ] change-finish ]
    [ '[ _ pos-rel- ] change-start nip ] 3bi ptext doc-become ;

: execute-parser ( word -- object/f )
    dup object>> \ parsers get ?at [ execute( -- parsed ) nip ] [ drop ] if ;

: parse-action ( string -- object )
    dup dup [ object>> ] when \ parsers get ?at [
        execute( -- parsed ) [ swap ptext doc-become prefix ] change-object
    ] [ drop ] if ;


: fixup-container-args ( type sep -- type' sep' )
    [ f like dup [ pexisting-class doc-become ] when ]
    [ [ 1string ] change-object ptext doc-become ] bi* ;

DEFER: parse-until

: parse-string' ( -- sep )
    "\\\"" read-until
    dup dup [ object>> ] when {
        { CHAR: " [ swap % ] }
        { CHAR: \ [ [ % ] [ , ] bi* read1 , parse-string' ] }
        { f [ [ dup [ object>> ] when ] dip tell-input double-quote-expected ] }
        [ tell-input double-quote-expected ]
    } case ;

: parse-string ( class sep -- mstring )
    fixup-container-args
    ptext doc-become
    tell-input [ parse-string' [ 1string ] change-object ptext doc-become ] "" make
    tell-input [ 1 - ] change-column rot [ pstring-text boa ] dip
    4 npick [ 4array ] [ 3array nip ] if
    pstring new swap >>object ;

: read-quotation ( -- quotation sep ) "]" parse-until ;

! : read-container ( class/f sep start-level -- container )
    ! "[" read-until rot CHAR: = <string> append "]" "]" surround {
        ! { [ ] [ ] }
    ! } cond ;

: parse-runtime-or-container ( class/f sep -- obj )
    fixup-container-args
    read1 {
        { [ dup object>> "\r\n\s" member? ] [ drop read-quotation 4array sift prun-time boa ] }
        ! { [ dup object>> CHAR: [ = ] [ 1 read-container ] }
        [ "\r\n\s" read-until drop 4array sift prun-time boa ]
    } cond ;


: class>trigger ( class/f -- trigger/f )
    ! name>> first "!@#$%^&*'`" member? ;
    dup [ object>> first "$" member? ] [ drop f ] if ;

CONSTANT: matching-syntax-table
    H{
        { CHAR: [ CHAR: ] }
        { CHAR: { CHAR: } }
        { CHAR: ( CHAR: ) }
        { CHAR: < CHAR: > }
        ! { CHAR: / CHAR: \ }
    }

ERROR: unimplemented pos str ;

ERROR: no-matching-syntax obj ch ;
: matching-syntax ( sep -- string )
    object>> dup first matching-syntax-table ?at [
        1string tuck '[
            _
            _ length 2 - CHAR: = <string>
            _
        ] "" append-outputs-as
    ] [
        no-matching-syntax
    ] if ;

ERROR: bracket-error pos ;
: (parse-trigger-bracket) ( ch -- obj )
    read1 {
        { f [ tell-input bracket-error ] }
        [ tell-input "here0" unimplemented ]
    } case ;


ERROR: malformed-bracket-opening sep pos ;

: verify-opening2 ( sep1 ch -- sep )
    2dup swap object>> = [ "error0" tell-input 2array throw ] unless
    [ [ start>> ] [ object>> ] bi ] dip
    ! pos ch ch
    2array >string tell-input ptext boa ;

:: verify-opening4 ( doc1 ch doc2 doc3 -- sep )
    doc1 object>>
    doc3 object>> first = [
        doc1 [ start>> ] [ object>> ] bi
        ch 2array
        doc2 object>>
        doc3 object>> "" 3append-as
        tell-input ptext boa
    ] [
        ! XXX: error
        "lol" throw
    ] if ;

:: word-combine ( class sep ch obj -- doc )
    class [
        class [ start>> ] [ object>> ] bi
        sep object>> suffix
        ch suffix
        obj [ object>> append ] [ finish>> ] bi \ doc boa
    ] [
        sep [ start>> ] [ object>> ] bi
        ch 2array >string
        obj [ object>> append ] [ finish>> ] bi \ doc boa
    ] if ;

ERROR: malformed-brace-opening sep pos ;

: parse-array ( class/f sep -- obj )
    [ 1string ] change-object ptext doc-become
    "}" parse-until 4array psequence boa ;

: parse-quotation ( class/f sep -- obj )
    [ 1string ] change-object ptext doc-become
    "]" parse-until 4array psequence boa ;

: parse-rest-of-opening ( sep ch -- full-opening-sep )
    2dup swap object>> = [
        verify-opening2
    ] [
        dup CHAR: = = [
            over object>> 1string multiline-string-until verify-opening4
        ] [
            "omgomgomg" throw
        ] if
    ] if ;

    ! dup {
        ! { f [ tell-input malformed-bracket-opening ] }
        ! { CHAR: = [ over object>> 1string multiline-string-until verify-opening4 ] }
        ! { CHAR: [ [ verify-opening2 ] }
    ! } case ;
 
: parse-contents-of-container ( class sep -- obj )
    dup matching-syntax multiline-string-until
    4array pcontainer boa ;

: parse-universal ( class sep ch -- obj )
    parse-rest-of-opening
    parse-contents-of-container ;

: parse-trigger-bracket ( contents ch -- obj )
    tell-input "here1" unimplemented ;
    ! '[ _ (parse-trigger-bracket) ] "" make ;

: parse-whitespace-bracket ( obj -- obj' )
    tell-input "here2" unimplemented ;

DEFER: raw
: parse-brace ( class/f sep -- obj )
    [ dup object>> empty? [ drop f ] when ] dip
    read1 dup [ object>> ] when {
        { [ dup f = ] [ tell-input "error10" 2array throw ] }
        { [ dup "\s\r\n" member? ] [ drop parse-array ] }
        { [ dup "={" member? ] [ parse-universal ] }
        [ raw word-combine ] ! just a word!?
    } cond ;

: parse-bracket ( class/f sep -- obj )
    [ dup object>> empty? [ drop f ] when ] dip
    read1 dup [ object>> ] when {
        { [ dup f = ] [ tell-input "error10" 2array throw ] }
        { [ dup "\s\r\n" member? ] [ drop parse-quotation ] }
        { [ dup "=[" member? ] [ parse-universal ] }
        [ raw word-combine ] ! just a word!?
    } cond ;


: token ( -- string/f )
    ! "\r\n\s\"" read-until {
    "\r\n\s\"{[" read-until {
        { [ dup f = ] [ drop ] } ! XXX: parse-action here?
        { [ dup object>> "\r\n\s" member? ] [ drop [ token ] when-empty ] }
        { [ dup object>> CHAR: " = ] [ parse-string ] }
        { [ dup object>> CHAR: { = ] [ parse-brace ] }
        { [ dup object>> CHAR: [ = ] [ parse-bracket ] }
    } cond ;

: typed-token ( type -- token/f )
    [ token ] dip doc-become ;

: raw ( -- object )
    "\r\n\s" read-until {
        { [ dup f = ] [ drop ] }
        { [ dup object>> "\r\n\s" member? ] [ drop [ raw ] when-empty ] }
    } cond ;

: raw-until ( string -- strings sep )
    dup '[
        raw
        [ dup object>> _ = [ ptext doc-become , f ] when ]
        [ _ tell-input token-expected ] if*
    ] loop>array unclip-last
    [ psequence boa ] [ ptext doc-become ] bi* ;

: typed-raw-until ( string type -- strings sep )
    [ raw-until ] dip '[ _ psequence-become ] dip ;

: new-class ( -- object ) pnew-class typed-token ;
: existing-class ( -- object ) pexisting-class typed-token ;
: new-word ( -- object ) pnew-word typed-token ;
: existing-word ( -- object ) pexisting-word typed-token ;
: token-to-find ( -- token string ) token [ ptext doc-become ] keep  ;
: parse ( -- object/f ) token dup [ parse-action ] when ;

! XXX: parsing word named ";" will execute while parse-until is looking for a ; -- may never find it!
! XXX: fix is to call token here and parse-action manually
: parse-until ( string -- strings sep )
    dup '[
        parse [ _ tell-input token-expected ] unless*
        dup object>> _ = [ , f ] when
    ] loop>array unclip-last
    [ psequence boa ] [ ptext doc-become ] bi* ;

: typed-parse-until ( string type -- strings sep )
    [ parse-until ] dip '[ _ psequence-become ] dip ;

: expect ( string -- string )
    ptext typed-token
    2dup dup [ object>> ] when =
    [ nip ]
    [ dup [ object>> ] when tell-input expected ] if ;

: body ( -- strings last ) ";" pbody typed-parse-until ;

: parse-input-stream ( -- seq ) [ parse ] loop>array ;


: parse-stream ( stream -- seq )
    [ parse-input-stream ] with-input-stream ; inline

: parse-source-file ( path -- data )
    utf8 [ input>document-stream parse-input-stream ] with-file-reader ; inline

: parse-modern-string ( string -- data )
    [ input>document-stream parse-input-stream ] with-string-reader ; inline

: parse-vocab ( string -- data )
    modern-source-path parse-source-file ;

: parse-metadata ( path -- data ) utf8 file-contents ;

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
M: doc write-parsed write ;
M: reldoc write-parsed write ;
M: psequence write-parsed object>> [ write-parsed ] each ;
M: string write-parsed <spaced-reldoc> write ;
M: sequence write-parsed [ write-parsed ] each ;

GENERIC: write-pflat' ( obj -- )
M: doc write-pflat' object>> write bl ;
M: psequence write-pflat' object>> [ write-parsed ] each ;

: write-pflat ( seq -- )
    [ write-pflat ] each nl ;

: write-pobjects ( seq -- )
    output>document-stream
    [ write-parsed ] each nl ;

: write-modern-string ( seq -- string )
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
        [
            [ name>> "parse-" prepend parser:create-word-in mark-top-level-syntax ]
            [ name>> "parse-" prepend "(" ")" surround parser:create-word-in ]
            [ ] tri
        ] 2dip
        ! word (word) class token quot
        {
            [ [ drop ] 3dip [ '[ _ expect ] ] dip compose swap '[ _ output>array _ boa ] ( -- obj ) define-declared ] ! word
            [ [ drop ] 4dip nip swap '[ _ output>array _ boa ] ( -- obj ) define-declared ] ! (word)
            [ [ drop ] 4dip drop nip register-parser ] ! (word) token register
        } 5 ncleave
    ] 3bi ;
>>

SYNTAX: PARSER:
    scan-new-class
    scan-token
    parser:parse-definition define-parser ;


! MOVE STUFF BELOW OUT

: reject-texts ( seq -- seq' ) [ ptext? ] reject ; inline
GENERIC: remove-texts ( obj -- obj' )
M: object remove-texts ;
M: sequence remove-texts [ remove-texts ] map ;
M: psequence remove-texts [ reject-texts [ remove-texts ] map ] change-object ;
