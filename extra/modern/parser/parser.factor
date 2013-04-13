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

! TODO: Save comments here
: parse-file ( path -- seq )
    dup >lower {
        { [ dup ".txt" tail? ] [ drop dup parse-metadata 2array ] }
        [ drop dup parse-source-file 2array ]
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

TUPLE: syntax name body ;
CONSTRUCTOR: syntax ( name body -- syntax ) ;
: parse-syntax ( -- syntax )
    raw body <syntax> ;

TUPLE: function name signature body ;
CONSTRUCTOR: function ( name signature body -- function ) ;
: parse-function ( -- function )
    token
    parse-signature(--)
    body <function> ;

TUPLE: locals-function name signature body ;
CONSTRUCTOR: locals-function ( name signature body -- function ) ;
: parse-locals-function ( -- function )
    token
    parse-signature(--)
    body <locals-function> ;

TUPLE: typed name signature body ;
CONSTRUCTOR: typed ( name signature body -- typed ) ;
: parse-typed ( -- function )
    token
    parse-signature(--)
    body <typed> ;

TUPLE: locals-typed name signature body ;
CONSTRUCTOR: locals-typed ( name signature body -- typed ) ;
: parse-locals-typed ( -- function )
    token
    parse-signature(--)
    body <locals-typed> ;


TUPLE: memo name signature body ;
CONSTRUCTOR: memo ( name signature body -- memo ) ;
: parse-memo ( -- function )
    token
    parse-signature(--)
    body <memo> ;

TUPLE: locals-memo name signature body ;
CONSTRUCTOR: locals-memo ( name signature body -- memo ) ;
: parse-locals-memo ( -- function )
    token
    parse-signature(--)
    body <locals-memo> ;


TUPLE: predicate name superclass body ;
CONSTRUCTOR: predicate ( name superclass body -- predicate ) ;
: parse-predicate ( -- predicate )
    token
    "<" expect
    token
    body <predicate> ;

TUPLE: slot name ;
CONSTRUCTOR: slot ( name -- slot ) ;
: parse-slot ( -- slot )
    token <slot> ;

TUPLE: specialized-array class ;
CONSTRUCTOR: specialized-array ( class -- speicialized-array ) ;
: parse-specialized-array ( -- slot )
    token <specialized-array> ;

TUPLE: specialized-arrays classes ;
CONSTRUCTOR: specialized-arrays ( classes -- speicialized-arrays ) ;
: parse-specialized-arrays ( -- slot )
    ";" strings-until <specialized-arrays> ;

TUPLE: postpone name ;
CONSTRUCTOR: postpone ( name -- postpone ) ;
: parse-postpone ( -- postpone )
    raw <postpone> ;

TUPLE: mixin name ;
CONSTRUCTOR: mixin ( name -- mixin ) ;
: parse-mixin ( -- mixin )
    token <mixin> ;

TUPLE: singleton name ;
CONSTRUCTOR: singleton ( name -- singleton ) ;
: parse-singleton ( -- singleton )
    token <singleton> ;

TUPLE: singletons names ;
CONSTRUCTOR: singletons ( names -- singletons ) ;
: parse-singletons ( -- singletons )
    body <singletons> ;

TUPLE: instance class mixin ;
CONSTRUCTOR: instance ( class mixin -- instance ) ;
: parse-instance ( -- instance )
    token token <instance> ;

TUPLE: use strings ;
CONSTRUCTOR: use ( strings -- use ) ;
: parse-use ( -- use ) token <use> ;

TUPLE: using strings ;
CONSTRUCTOR: using ( strings -- use ) ;
: parse-using ( -- using ) ";" strings-until <using> ;

TUPLE: author name ;
CONSTRUCTOR: author ( name -- author ) ;
: parse-author ( -- author )
    string-until-eol [ " " member? ] trim <author> ;

TUPLE: block body ;
CONSTRUCTOR: block ( body -- block ) ;
: parse-block ( -- block )
    "]" parse-until <block> ;

TUPLE: parsetime-block body ;
CONSTRUCTOR: parsetime-block ( body -- parsetime-block ) ;
: parse-parsetime-block ( -- block )
    "]" parse-until <parsetime-block> ;

TUPLE: locals-block body ;
CONSTRUCTOR: locals-block ( body -- block ) ;
: parse-locals-block ( -- block )
    "]" parse-until <locals-block> ;

TUPLE: single-bind target ;
TUPLE: multi-bind targets ;
CONSTRUCTOR: single-bind ( target -- bind ) ;
CONSTRUCTOR: multi-bind ( targets -- bind ) ;
: parse-bind ( -- bind )
    raw dup "(" = [
        drop ")" strings-until <multi-bind>
    ] [
        <single-bind>
    ] if ;

TUPLE: fry body ;
CONSTRUCTOR: fry ( body -- block ) ;
: parse-fry ( -- block )
    "]" parse-until <fry> ;

TUPLE: marray elements ;
CONSTRUCTOR: marray ( elements -- block ) ;
: parse-marray ( -- block )
    "}" parse-until <marray> ;

TUPLE: mvector elements ;
CONSTRUCTOR: mvector ( elements -- block ) ;
: parse-mvector ( -- block )
    "}" parse-until <mvector> ;

TUPLE: mhashtable elements ;
CONSTRUCTOR: mhashtable ( elements -- block ) ;
: parse-mhashtable ( -- block )
    "}" parse-until <mhashtable> ;

TUPLE: tuple-literal-assoc name slots ;
TUPLE: tuple-literal-boa name slots ;
CONSTRUCTOR: tuple-literal-assoc ( name slots -- tuple-literal ) ;
CONSTRUCTOR: tuple-literal-boa ( name slots -- tuple-literal ) ;
: parse-tuple-literal ( -- block )
    token
    token dup {
        { "f" [ drop "}" parse-until <tuple-literal-boa> ] }
        { "{" [
                  drop parse-marray
                  "}" parse-until swap prefix <tuple-literal-assoc>
              ]
        }
        { "}" [ drop f <tuple-literal-boa> ] }
    } case ;

TUPLE: char n ;
CONSTRUCTOR: char ( n -- char ) ;
: parse-char ( -- char )
    raw <char> ;

TUPLE: in name ;
CONSTRUCTOR: in ( name -- in ) ;
: parse-in ( -- in )
    token <in> ;

TUPLE: main name ;
CONSTRUCTOR: main ( name -- main ) ;
: parse-main ( -- main )
    token <main> ;

TUPLE: escaped name ;
CONSTRUCTOR: escaped ( name -- escaped ) ;
: parse-escaped ( -- escaped )
    raw <escaped> ;

TUPLE: execute( signature ;
CONSTRUCTOR: execute( ( signature -- execute ) ;
: parse-execute( ( -- execute( )
    parse-signature--) <execute(> ;

TUPLE: call( signature ;
CONSTRUCTOR: call( ( signature -- call ) ;
: parse-call( ( -- call( )
    parse-signature--) <call(> ;

TUPLE: data-map( signature ;
CONSTRUCTOR: data-map( ( signature -- data-map ) ;
: parse-data-map( ( -- call( )
    parse-signature--) <data-map(> ;

TUPLE: data-map!( signature ;
CONSTRUCTOR: data-map!( ( signature -- data-map! ) ;
: parse-data-map!( ( -- call( )
    parse-signature--) <data-map!(> ;

TUPLE: hints name sequence ;
CONSTRUCTOR: hints ( name sequence -- hints ) ;
: parse-hints ( -- generic )
    token body <hints> ;

TUPLE: mgeneric name signature ;
CONSTRUCTOR: mgeneric ( name signature -- generic ) ;
: parse-mgeneric ( -- generic )
    token parse-signature(--) <mgeneric> ;

TUPLE: hook name symbol signature ;
CONSTRUCTOR: hook ( name symbol signature -- hook ) ;
: parse-hook ( -- hook )
    token token parse-signature(--) <hook> ;

TUPLE: mgeneric# name n signature ;
CONSTRUCTOR: mgeneric# ( name n signature -- generic ) ;
: parse-mgeneric# ( -- generic )
    token token parse-signature(--) <mgeneric#> ;

TUPLE: mmethod class name body ;
CONSTRUCTOR: mmethod ( class name body -- method ) ;
: parse-mmethod ( -- method )
    parse token body <mmethod> ;

TUPLE: locals-mmethod class name body ;
CONSTRUCTOR: locals-mmethod ( class name body -- locals-method ) ;
: parse-locals-mmethod ( -- method )
    parse token body <locals-mmethod> ;

TUPLE: constructor name class ;
CONSTRUCTOR: constructor ( name class -- constructor ) ;
: parse-constructor ( -- constructor )
    token token <constructor> ;

TUPLE: private body ;
CONSTRUCTOR: private ( body -- private ) ;
: parse-private ( -- private )
    "PRIVATE>" parse-until <private> ;

TUPLE: from module functions ;
CONSTRUCTOR: from ( module functions -- from ) ;
: parse-from ( -- from )
    token ";" strings-until <from> ;

TUPLE: qualified name ;
CONSTRUCTOR: qualified ( name -- qualified ) ;
: parse-qualified ( -- qualified )
    token <qualified> ;

TUPLE: qualified-with name prefix ;
CONSTRUCTOR: qualified-with ( name prefix -- qualified-with ) ;
: parse-qualified-with ( -- qualified-with )
    token token <qualified-with> ;

TUPLE: constant name object ;
CONSTRUCTOR: constant ( name object -- constant ) ;
: parse-constant ( -- constant )
    token parse <constant> ;

TUPLE: mtuple name body ;
CONSTRUCTOR: mtuple ( name body -- tuple ) ;
: parse-mtuple ( -- mtuple )
    token body <mtuple> ;

TUPLE: merror name body ;
CONSTRUCTOR: merror ( name body -- error ) ;
: parse-merror ( -- merror )
    token body <merror> ;

TUPLE: mparser name start slots body ;
CONSTRUCTOR: mparser ( name start slots body -- mparser ) ;
: parse-mparser ( -- mparser )
    get-string parse parse body <mparser> ;

TUPLE: mprimitive name signature ;
CONSTRUCTOR: mprimitive ( name signature -- package ) ;
: parse-mprimitive ( -- mprimitive )
    parse parse-signature(--) <mprimitive> ;

TUPLE: package name ;
CONSTRUCTOR: package ( name -- package ) ;
: parse-package ( -- package )
    get-string <package> ;

TUPLE: import name ;
CONSTRUCTOR: import ( name -- package ) ;
: parse-import ( -- import )
    get-string <import> ;

TUPLE: imports names ;
CONSTRUCTOR: imports ( names -- package ) ;
: parse-imports ( -- import )
    ";" strings-until <imports> ;

TUPLE: foldable ;
CONSTRUCTOR: foldable ( -- obj ) ;
: parse-foldable ( -- foldable ) <foldable> ;

TUPLE: inline ;
CONSTRUCTOR: inline ( -- obj ) ;
: parse-inline ( -- inline ) <inline> ;

TUPLE: final ;
CONSTRUCTOR: final ( -- obj ) ;
: parse-final ( -- final ) <final> ;

TUPLE: recursive ;
CONSTRUCTOR: recursive ( -- obj ) ;
: parse-recursive ( -- recursive ) <recursive> ;

TUPLE: union name strings ;
CONSTRUCTOR: union ( name strings -- obj ) ;
: parse-union ( -- recursive )
    token body <union> ;

TUPLE: flushable ;
CONSTRUCTOR: flushable ( -- obj ) ;
: parse-flushable ( -- flushable ) <flushable> ;

TUPLE: mbuiltin name body ;
CONSTRUCTOR: mbuiltin ( name body -- obj ) ;
: parse-mbuiltin ( -- builtin )
    token body <mbuiltin> ;

TUPLE: math name body ;
CONSTRUCTOR: math ( name body -- obj ) ;
: parse-math ( -- builtin )
    token parse-signature(--) <math> ;

TUPLE: functor name signature definitions ;
CONSTRUCTOR: functor ( name signature definitions -- functor ) ;
: parse-functor ( -- functor )
    token parse-signature(--) ";FUNCTOR" parse-until <functor> ;

TUPLE: name name target ;
CONSTRUCTOR: name ( name target -- object ) ;
: parse-name ( -- name )
    token token <name> ;

TUPLE: ebnf text ;
CONSTRUCTOR: ebnf ( text -- ebnf ) ;
: parse-ebnf ( -- ebnf )
    ";EBNF" strings-until <ebnf> ;

TUPLE: defer name ;
CONSTRUCTOR: defer ( name -- defer ) ;
: parse-defer ( -- defer )
    token <defer> ;

TUPLE: symbol name ;
CONSTRUCTOR: symbol ( name -- symbol ) ;
: parse-symbol ( -- symbol )
    token <symbol> ;

TUPLE: symbols names ;
CONSTRUCTOR: symbols ( names -- symbols ) ;
: parse-symbols ( -- symbols )
    ";" parse-until <symbols> ;

TUPLE: compilation-unit code ;
CONSTRUCTOR: compilation-unit ( code -- compilation-unit ) ;
: parse-compilation-unit ( -- compilation-unit )
    ">>" parse-until <compilation-unit> ;

TUPLE: rename function module name ;
CONSTRUCTOR: rename ( function module name -- rename ) ;
: parse-rename ( -- renamed )
    token token "=>" expect token <rename> ;

TUPLE: typedef old new ;
CONSTRUCTOR: typedef ( old new -- typedef ) ;
: parse-typedef ( -- typedef )
    token token <typedef> ;

TUPLE: library name ;
CONSTRUCTOR: library ( name -- library ) ;
: parse-library ( -- library )
    token <library> ;

TUPLE: c-function return-value name arguments ;
CONSTRUCTOR: c-function ( return-value name arguments -- c-function ) ;
: parse-c-function ( -- c-function )
    token token ";" parse-until <c-function> ;

TUPLE: x-function return-value name arguments ;
CONSTRUCTOR: x-function ( return-value name arguments -- c-function ) ;
: parse-x-function ( -- c-function )
    token token ";" parse-until <x-function> ;

TUPLE: c-function-alias aliased-name return-value name arguments ;
CONSTRUCTOR: c-function-alias ( aliased-name return-value name arguments -- c-function ) ;
: parse-c-function-alias ( -- c-function )
    token token token ";" parse-until <c-function-alias> ;

TUPLE: gl-function return-value name arguments ;
CONSTRUCTOR: gl-function ( return-value name arguments -- gl-function ) ;
: parse-gl-function ( -- gl-function )
    token token ";" parse-until <gl-function> ;

TUPLE: c-type name ;
CONSTRUCTOR: c-type ( name -- c-type ) ;
: parse-c-type ( -- c-type )
    token <c-type> ;

TUPLE: macro name signature body ;
CONSTRUCTOR: macro ( name signature body -- macro ) ;
: parse-macro ( -- macro )
    token parse-signature(--) ";" parse-until <macro> ;

TUPLE: locals-macro name signature body ;
CONSTRUCTOR: locals-macro ( name signature body -- macro ) ;
: parse-locals-macro ( -- macro )
    token parse-signature(--) ";" parse-until <locals-macro> ;

TUPLE: struct name slots ;
CONSTRUCTOR: struct ( name slots -- struct ) ;
: parse-struct ( -- struct )
    token ";" parse-until <struct> ;

TUPLE: packed-struct name slots ;
CONSTRUCTOR: packed-struct ( name slots -- struct ) ;
: parse-packed-struct ( -- struct )
    token ";" parse-until <packed-struct> ;

TUPLE: alias name target ;
CONSTRUCTOR: alias ( name target -- alias ) ;
: parse-alias ( -- alias )
    token token <alias> ;

TUPLE: registers names ;
CONSTRUCTOR: registers ( names -- obj ) ;
: parse-registers ( -- obj )
    ";" parse-until <registers> ;

TUPLE: hi-registers names ;
CONSTRUCTOR: hi-registers ( names -- obj ) ;
: parse-hi-registers ( -- obj )
    ";" parse-until <hi-registers> ;

TUPLE: about name ;
CONSTRUCTOR: about ( name -- obj ) ;
: parse-about ( -- obj )
    token <about> ;

TUPLE: article name body ;
CONSTRUCTOR: article ( name body -- obj ) ;
: parse-article ( -- obj )
    token ";" parse-until <article> ;

TUPLE: c-global name ;
CONSTRUCTOR: c-global ( name -- obj ) ;
: parse-c-global ( -- obj )
    token <c-global> ;

TUPLE: protocol name functions ;
CONSTRUCTOR: protocol ( name functions -- obj ) ;
: parse-protocol ( -- obj )
    token ";" parse-until <protocol> ;

TUPLE: tr name body ;
CONSTRUCTOR: tr ( name body -- obj ) ;
: parse-tr ( -- obj )
    token ";" parse-until <tr> ;

TUPLE: exclude name body ;
CONSTRUCTOR: exclude ( name body -- obj ) ;
: parse-exclude ( -- obj )
    token "=>" expect ";" parse-until <exclude> ;

TUPLE: foldable-insn name body ;
CONSTRUCTOR: foldable-insn ( name body -- obj ) ;
: parse-foldable-insn ( -- obj )
    token ";" parse-until <foldable-insn> ;

TUPLE: flushable-insn name body ;
CONSTRUCTOR: flushable-insn ( name body -- obj ) ;
: parse-flushable-insn ( -- obj )
    token ";" parse-until <flushable-insn> ;

TUPLE: vreg-insn name body ;
CONSTRUCTOR: vreg-insn ( name body -- obj ) ;
: parse-vreg-insn ( -- obj )
    token ";" parse-until <vreg-insn> ;

TUPLE: insn name body ;
CONSTRUCTOR: insn ( name body -- obj ) ;
: parse-insn ( -- obj )
    token ";" parse-until <insn> ;

TUPLE: codegen name1 name2 ;
CONSTRUCTOR: codegen ( name1 name2 -- obj ) ;
: parse-codegen ( -- obj )
    token token <codegen> ;

TUPLE: conditional name1 name2 ;
CONSTRUCTOR: conditional ( name1 name2 -- obj ) ;
: parse-conditional ( -- obj )
    token token <conditional> ;

TUPLE: simd-128 name ;
CONSTRUCTOR: simd-128 ( name -- obj ) ;
: parse-simd-128 ( -- obj )
    token <simd-128> ;

TUPLE: simd-128-cord name1 name2 ;
CONSTRUCTOR: simd-128-cord ( name1 name2 -- obj ) ;
: parse-simd-128-cord ( -- obj )
    token token <simd-128-cord> ;

TUPLE: simd-intrinsic name body ;
CONSTRUCTOR: simd-intrinsic ( name body -- obj ) ;
: parse-simd-intrinsic ( -- obj )
    token ";" parse-until <simd-intrinsic> ;

TUPLE: locals-simd-intrinsic name body ;
CONSTRUCTOR: locals-simd-intrinsic ( name body -- obj ) ;
: parse-locals-simd-intrinsic ( -- obj )
    token ";" parse-until <locals-simd-intrinsic> ;

TUPLE: enum name slots ;
CONSTRUCTOR: enum ( name slots -- obj ) ;
: parse-enum ( -- obj )
    token ";" parse-until <enum> ;

TUPLE: forget name ;
CONSTRUCTOR: forget ( name -- obj ) ;
: parse-forget ( -- obj )
    token <forget> ;

TUPLE: pointer to ;
CONSTRUCTOR: pointer ( to -- obj ) ;
: parse-pointer ( -- obj )
    token <pointer> ;

TUPLE: help body ;
CONSTRUCTOR: help ( body -- obj ) ;
: parse-help ( -- help )
    ";" parse-until <help> ;

\ parse-mparser "PARSER:" parsers get set-at
\ parse-package "PACKAGE:" parsers get set-at
\ parse-import "IMPORT:" parsers get set-at
\ parse-imports "IMPORTS:" parsers get set-at
\ parse-author "AUTHOR:" parsers get set-at
\ parse-from "FROM:" parsers get set-at
\ parse-qualified "QUALIFIED:" parsers get set-at
\ parse-qualified-with "QUALIFIED-WITH:" parsers get set-at
\ parse-use "USE:" parsers get set-at
\ parse-using "USING:" parsers get set-at
\ parse-in "IN:" parsers get set-at
\ parse-main "MAIN:" parsers get set-at
\ parse-mbuiltin "BUILTIN:" parsers get set-at
\ parse-math "MATH:" parsers get set-at
\ parse-union "UNION:" parsers get set-at
\ parse-char "CHAR:" parsers get set-at
\ parse-escaped "\\" parsers get set-at
\ parse-execute( "execute(" parsers get set-at
\ parse-call( "call(" parsers get set-at
\ parse-data-map( "data-map(" parsers get set-at
\ parse-data-map!( "data-map!(" parsers get set-at
\ parse-private "<PRIVATE" parsers get set-at
\ parse-constant "CONSTANT:" parsers get set-at
\ parse-mtuple "TUPLE:" parsers get set-at
\ parse-merror "ERROR:" parsers get set-at
\ parse-mprimitive "PRIMITIVE:" parsers get set-at
\ parse-foldable "foldable" parsers get set-at
\ parse-flushable "flushable" parsers get set-at
\ parse-inline "inline" parsers get set-at
\ parse-final "final" parsers get set-at
\ parse-recursive "recursive" parsers get set-at
\ parse-block "[" parsers get set-at
\ parse-parsetime-block "$[" parsers get set-at
\ parse-locals-block "[|" parsers get set-at
\ parse-bind ":>" parsers get set-at
\ parse-fry "'[" parsers get set-at
\ parse-marray "{" parsers get set-at
\ parse-mvector "V{" parsers get set-at
\ parse-mhashtable "H{" parsers get set-at
\ parse-tuple-literal "T{" parsers get set-at
\ parse-mgeneric "GENERIC:" parsers get set-at
\ parse-mgeneric# "GENERIC#" parsers get set-at
\ parse-mmethod "M:" parsers get set-at
\ parse-locals-mmethod "M::" parsers get set-at
\ parse-constructor "C:" parsers get set-at
\ parse-function ":" parsers get set-at
\ parse-locals-function "::" parsers get set-at
\ parse-typed "TYPED:" parsers get set-at
\ parse-locals-typed "TYPED::" parsers get set-at
\ parse-memo "MEMO:" parsers get set-at
\ parse-locals-memo "MEMO::" parsers get set-at
\ parse-instance "INSTANCE:" parsers get set-at
\ parse-predicate "PREDICATE:" parsers get set-at
\ parse-slot "SLOT:" parsers get set-at
\ parse-mixin "MIXIN:" parsers get set-at
\ parse-singleton "SINGLETON:" parsers get set-at
\ parse-singletons "SINGLETONS:" parsers get set-at
\ parse-comment "!" parsers get set-at
\ parse-comment "#!" parsers get set-at
\ parse-nested-comment "(*" parsers get set-at
\ parse-nested-comment "(*" comment-parsers get set-at
\ parse-postpone "POSTPONE:" parsers get set-at
\ parse-hints "HINTS:" parsers get set-at
\ parse-specialized-array "SPECIALIZED-ARRAY:" parsers get set-at
\ parse-specialized-arrays "SPECIALIZED-ARRAYS:" parsers get set-at
\ parse-syntax "SYNTAX:" parsers get set-at
\ parse-functor "FUNCTOR:" parsers get set-at
\ parse-name "NAME:" parsers get set-at
\ parse-ebnf "EBNF:" parsers get set-at
\ parse-symbol "SYMBOL:" parsers get set-at
\ parse-symbols "SYMBOLS:" parsers get set-at
\ parse-defer "DEFER:" parsers get set-at
\ parse-hook "HOOK:" parsers get set-at
\ parse-compilation-unit "<<" parsers get set-at
\ parse-rename "RENAME:" parsers get set-at
\ parse-typedef "TYPEDEF:" parsers get set-at
\ parse-c-function "FUNCTION:" parsers get set-at
\ parse-x-function "X-FUNCTION:" parsers get set-at
\ parse-c-function-alias "FUNCTION-ALIAS:" parsers get set-at
\ parse-gl-function "GL-FUNCTION:" parsers get set-at
\ parse-library "LIBRARY:" parsers get set-at
\ parse-c-type "C-TYPE:" parsers get set-at
\ parse-macro "MACRO:" parsers get set-at
\ parse-locals-macro "MACRO::" parsers get set-at
\ parse-struct "STRUCT:" parsers get set-at
\ parse-packed-struct "PACKED-STRUCT:" parsers get set-at
\ parse-alias "ALIAS:" parsers get set-at
\ parse-registers "REGISTERS:" parsers get set-at
\ parse-hi-registers "HI-REGISTERS:" parsers get set-at
\ parse-about "ABOUT:" parsers get set-at
\ parse-c-global "C-GLOBAL:" parsers get set-at
\ parse-article "ARTICLE:" parsers get set-at
\ parse-protocol "PROTOCOL:" parsers get set-at
\ parse-exclude "EXCLUDE:" parsers get set-at
\ parse-foldable-insn "FOLDABLE-INSN:" parsers get set-at
\ parse-flushable-insn "FLUSHABLE-INSN:" parsers get set-at
\ parse-insn "INSN:" parsers get set-at
\ parse-vreg-insn "VREG-INSN:" parsers get set-at
\ parse-codegen "CODEGEN:" parsers get set-at
\ parse-conditional "CONDITIONAL:" parsers get set-at
\ parse-simd-128 "SIMD-128:" parsers get set-at
\ parse-simd-128-cord "SIMD-128-CORD:" parsers get set-at
\ parse-simd-intrinsic "SIMD-INTRINSIC:" parsers get set-at
\ parse-locals-simd-intrinsic "SIMD-INTRINSIC::" parsers get set-at
\ parse-enum "ENUM:" parsers get set-at
\ parse-forget "FORGET:" parsers get set-at
\ parse-pointer "pointer:" parsers get set-at
! \ parse-string "STRING:" parsers get set-at
\ parse-help "HELP:" parsers get set-at

! qw{ CODEGEN: CATEGORY: ENUM: SIMD-INTRINSIC:
! CLASS: FUNCTION-ALIAS: M\\ TAGS: COM-INTERFACE:

! "EUC:" "GIR:" "8-BIT:" "CLASS:" "ENUM:" "RENAMING:" "TEST:" "CHLOE:" "FORGET:" "XML-NS:"
! "BEFORE:" "AFTER:" "IMPLEMENT-STRUCTS:" "DESTRUCTOR:" "FOREIGN-RECORD-TYPE:" "FOREIGN-ENUM-TYPE:" "FORWARD-ANALYSIS:"
! "COM-INTERFACE:" "INTERSECTION:" "LOG:" "CFSTRING:" "CONSULT:" "PEG:" "BACKWARD-ANALYSIS:" "MACRO::"
! "UNION-STRUCT:" "PACKED-STRUCT:" "SLOT-PROTOCOL:" "ICON:" "TAGS:" "TAG:" "ROMAN-OP:"
! "STRING:" "SIMD-128:" "MATCH-VARS:" "CALLBACK:" "CATEGORY:" "PIXEL-FORMAT-ATTRIBUTE-TABLE:" "pointer:"
! "CATEGORY-NOT:" "FUNCTOR-SYNTAX:" "IDENTITY-MEMO:" "XML-ERROR:" "RULE:" "COMPONENT:" "X509_V_:"
! "X-FUNCTION:" "SIMD-INTRINSIC:" "SIMD-INTRINSIC::"

! "MAIN-WINDOW:" "GIR:" "TUPLE-ARRAY:" "VARIANT:" "SPECIALIZED-VECTOR:" "FOREIGN-RECORD-TYPE:" "FOREIGN-ATOMIC-TYPE:"
! "SOLUTION:" "CUDA-LIBRARY:" "CUDA-FUNCTION:" "HOLIDAY:" "HOLIDAY-NAME:" "FRAMEWORK:" "GAME:"
! "INTERSECTION:" "DESTRUCTOR:" "FORWARD-ANALYSIS:" "RENAMING:" "MACRO::" "XML-NS:" "CALLBACK:" "GLSL-SHADER:"
! "UNIFORM-TUPLE:" "GLSL-PROGRAM:" "LITERAL-PARSER:" "CONSTRUCTOR:" "STRING:" "ENUM:" "GLSL-SHADER-FILE:"
! "AFTER:" "system-attachment:" "initial:" "color-attachment:" "CFSTRING:" "DERIVATIVE:" "FUNCTOR-SYNTAX:"
! "VERTEX-FORMAT:" "SELECTOR:" "REGISTER:" "METHOD:" "TODO:" "LAZY:"
! "ROLE:" "UNION-STRUCT:" "INSTRUCTION:" "SUBROUTINE:"

! "resource:core" vocabs-in-root [ vocab? ] filter [ vocab-files ] map concat
! "resource:core" vocabs-in-root [ vocab? ] filter [ vocab-files ] map concat
! [ "-docs.factor" tail? not ] filter
! [ "-tests.factor" tail? not ] filter [ parse-file ] map

! "resource:basis" vocabs-in-root [ vocab? ] filter [ vocab-files ] map concat
! [ "-docs.factor" tail? not ] filter
! [ "-tests.factor" tail? not ] filter
! { "resource:basis/tools/completion/completion.factor"
! "resource:basis/functors/functors.factor" "resource:basis/ui/tools/listener/completion/completion.factor"
! "resource:basis/simple-tokenizer/simple-tokenizer.factor" } diff
! [ dup . flush parse-file drop ] map
