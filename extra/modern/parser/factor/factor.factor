! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs combinators constructors io kernel make
modern.parser multiline namespaces nested-comments sequences ;
IN: modern.parser.factor

TUPLE: mparser < parsed name start slots body ;
CONSTRUCTOR: <mparser> mparser ( name slots start body -- mparser ) ;
: parse-parser ( -- mparser )
    raw parse raw body <mparser> ;

TUPLE: text < parsed string from to ;
CONSTRUCTOR: <text> text ( string from to -- text ) ;

TUPLE: mnested-comment < parsed comment ;
CONSTRUCTOR: <mnested-comment> mnested-comment ( comment -- nested-comment ) ;
: parse-nested-comment ( -- nested-comment )
    "*)" parse-comment-until <mnested-comment> ;

TUPLE: signature < parsed in out ;
CONSTRUCTOR: <signature> signature ( in out -- signature ) ;

TUPLE: typed-argument < parsed name signature ;
CONSTRUCTOR: <typed-argument> typed-argument ( name signature -- typed ) ;

DEFER: parse-signature(--)
DEFER: parse-nested-signature(--)
DEFER: parse-signature-in
DEFER: parse-signature-in'

ERROR: signature-expected position ;
: parse-signature-in'' ( -- parse-out? )
    raw [ tell-input signature-expected ] unless*
    dup ":" tail? [
        parse-nested-signature(--) <typed-argument> ,
        parse-signature-in''
    ] [
        {
            { "--" [ t ] }
            { ")" [ f ] }
            [ , parse-signature-in'' ]
        } case
    ] if ;

: parse-signature-in' ( -- in parse-out? )
    [ parse-signature-in'' ] { } make swap ;

: parse-signature-in ( -- in ? )
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

: maybe-parse-out ( ? -- out/f )
    [ parse-signature-out ] [ f ] if ;

: parse-nested-signature(--) ( -- signature )
    raw dup "(" = [
        drop
        parse-signature-in' maybe-parse-out <signature>
    ] when ;

: parse-signature(--) ( -- signature )
    parse-signature-in maybe-parse-out <signature> ;

: parse-signature--) ( -- signature )
    parse-signature-in' maybe-parse-out <signature> ;

: c-arguments ( -- arguments )
    "(" expect
    ")" strings-until ;

TUPLE: syntax < parsed name body ;
CONSTRUCTOR: <syntax> syntax ( name body -- syntax ) ;
: parse-syntax ( -- syntax )
    raw body <syntax> ;

TUPLE: function < parsed name signature body ;
CONSTRUCTOR: <function> function ( name signature body -- function ) ;
: parse-function ( -- function )
    token
    parse-signature(--)
    body <function> ;

TUPLE: locals-function < parsed name signature body ;
CONSTRUCTOR: <locals-function> locals-function ( name signature body -- function ) ;
: parse-locals-function ( -- function )
    token
    parse-signature(--)
    body <locals-function> ;

TUPLE: typed < parsed name signature body ;
CONSTRUCTOR: <typed> typed ( name signature body -- typed ) ;
: parse-typed ( -- function )
    token
    parse-signature(--)
    body <typed> ;

TUPLE: locals-typed < parsed name signature body ;
CONSTRUCTOR: <locals-typed> locals-typed ( name signature body -- typed ) ;
: parse-locals-typed ( -- function )
    token
    parse-signature(--)
    body <locals-typed> ;


TUPLE: memo < parsed name signature body ;
CONSTRUCTOR: <memo> memo ( name signature body -- memo ) ;
: parse-memo ( -- function )
    token
    parse-signature(--)
    body <memo> ;

TUPLE: locals-memo < parsed name signature body ;
CONSTRUCTOR: <locals-memo> locals-memo ( name signature body -- memo ) ;
: parse-locals-memo ( -- function )
    token
    parse-signature(--)
    body <locals-memo> ;


TUPLE: predicate < parsed name superclass body ;
CONSTRUCTOR: <predicate> predicate ( name superclass body -- predicate ) ;
: parse-predicate ( -- predicate )
    token
    "<" expect
    token
    body <predicate> ;

TUPLE: slot < parsed name ;
CONSTRUCTOR: <slot> slot ( name -- slot ) ;
: parse-slot ( -- slot )
    token <slot> ;

TUPLE: specialized-array < parsed class ;
CONSTRUCTOR: <specialized-array> specialized-array ( class -- speicialized-array ) ;
: parse-specialized-array ( -- slot )
    token <specialized-array> ;

TUPLE: specialized-arrays < parsed classes ;
CONSTRUCTOR: <specialized-arrays> specialized-arrays ( classes -- speicialized-arrays ) ;
: parse-specialized-arrays ( -- slot )
    ";" strings-until <specialized-arrays> ;

TUPLE: postpone < parsed  name ;
CONSTRUCTOR: <postpone> postpone ( name -- postpone ) ;
: parse-postpone ( -- postpone )
    raw <postpone> ;

TUPLE: mixin < parsed  name ;
CONSTRUCTOR: <mixin> mixin ( name -- mixin ) ;
: parse-mixin ( -- mixin )
    token <mixin> ;

TUPLE: singleton < parsed name ;
CONSTRUCTOR: <singleton> singleton ( name -- singleton ) ;
: parse-singleton ( -- singleton )
    token <singleton> ;

TUPLE: singletons < parsed names ;
CONSTRUCTOR: <singletons> singletons ( names -- singletons ) ;
: parse-singletons ( -- singletons )
    body <singletons> ;

TUPLE: instance < parsed class mixin ;
CONSTRUCTOR: <instance> instance ( class mixin -- instance ) ;
: parse-instance ( -- instance )
    token token <instance> ;

TUPLE: use < parsed strings ;
CONSTRUCTOR: <use> use ( strings -- use ) ;
: parse-use ( -- use ) token <use> ;

TUPLE: using < parsed strings ;
CONSTRUCTOR: <using> using ( strings -- use ) ;
: parse-using ( -- using ) ";" strings-until <using> ;

TUPLE: author < parsed name ;
CONSTRUCTOR: <author> author ( name -- author ) ;
: parse-author ( -- author )
    string-until-eol [ " " member? ] trim <author> ;

TUPLE: block < parsed body ;
CONSTRUCTOR: <block> block ( body -- block ) ;
: parse-block ( -- block )
    "]" parse-until <block> ;

TUPLE: parsetime-block < parsed body ;
CONSTRUCTOR: <parsetime-block> parsetime-block ( body -- parsetime-block ) ;
: parse-parsetime-block ( -- block )
    "]" parse-until <parsetime-block> ;

TUPLE: locals-block < parsed body ;
CONSTRUCTOR: <locals-block> locals-block ( body -- block ) ;
: parse-locals-block ( -- block )
    "]" parse-until <locals-block> ;

TUPLE: single-bind < parsed target ;
TUPLE: multi-bind < parsed targets ;
CONSTRUCTOR: <single-bind> single-bind ( target -- bind ) ;
CONSTRUCTOR: <multi-bind> multi-bind ( targets -- bind ) ;
: parse-bind ( -- bind )
    raw dup "(" = [
        drop ")" strings-until <multi-bind>
    ] [
        <single-bind>
    ] if ;

TUPLE: mfry < parsed body ;
CONSTRUCTOR: <mfry> mfry ( body -- block ) ;
: parse-fry ( -- block )
    "]" parse-until <mfry> ;

TUPLE: marray < parsed elements ;
CONSTRUCTOR: <marray> marray ( elements -- block ) ;
: parse-marray ( -- block )
    "}" parse-until <marray> ;

TUPLE: mvector < parsed elements ;
CONSTRUCTOR: <mvector> mvector ( elements -- block ) ;
: parse-mvector ( -- block )
    "}" parse-until <mvector> ;

TUPLE: mhashtable < parsed elements ;
CONSTRUCTOR: <mhashtable> mhashtable ( elements -- block ) ;
: parse-mhashtable ( -- block )
    "}" parse-until <mhashtable> ;

TUPLE: tuple-literal-assoc < parsed name slots ;
TUPLE: tuple-literal-boa < parsed name slots ;
CONSTRUCTOR: <tuple-literal-assoc> tuple-literal-assoc ( name slots -- tuple-literal ) ;
CONSTRUCTOR: <tuple-literal-boa> tuple-literal-boa ( name slots -- tuple-literal ) ;

ERROR: malformed-tuple-literal ;
: parse-tuple-literal ( -- block )
    token
    token dup dup [ name>> ] when {
        { "f" [ drop "}" parse-until <tuple-literal-boa> ] }
        { "{" [
                  drop parse-marray
                  "}" parse-until swap prefix <tuple-literal-assoc>
              ]
        }
        { "}" [ drop f <tuple-literal-boa> ] }
        [ malformed-tuple-literal ]
    } case ;

TUPLE: char < parsed n ;
CONSTRUCTOR: <char> char ( n -- char ) ;
: parse-char ( -- char )
    raw <char> ;

TUPLE: min < parsed name ;
CONSTRUCTOR: <min> min ( name -- in ) ;
: parse-in ( -- in )
    token <min> ;

TUPLE: main < parsed name ;
CONSTRUCTOR: <main> main ( name -- main ) ;
: parse-main ( -- main )
    token <main> ;

TUPLE: escaped < parsed name ;
CONSTRUCTOR: <escaped> escaped ( name -- escaped ) ;
: parse-escaped ( -- escaped )
    raw <escaped> ;

TUPLE: mexecute( < parsed signature ;
CONSTRUCTOR: <mexecute(> mexecute( ( signature -- execute ) ;
: parse-execute( ( -- execute( )
    parse-signature--) <mexecute(> ;

TUPLE: mcall( < parsed signature ;
CONSTRUCTOR: <mcall(> mcall( ( signature -- call ) ;
: parse-call( ( -- call( )
    parse-signature--) <mcall(> ;

TUPLE: mdata-map( < parsed signature ;
CONSTRUCTOR: <mdata-map(> mdata-map( ( signature -- data-map ) ;
: parse-data-map( ( -- obj )
    parse-signature--) <mdata-map(> ;

TUPLE: mdata-map!( < parsed signature ;
CONSTRUCTOR: <mdata-map!(> mdata-map!( ( signature -- data-map! ) ;
: parse-data-map!( ( -- obj )
    parse-signature--) <mdata-map!(> ;

TUPLE: hints < parsed name sequence ;
CONSTRUCTOR: <hints> hints ( name sequence -- hints ) ;
: parse-hints ( -- generic )
    parse body <hints> ;

TUPLE: mgeneric < parsed name signature ;
CONSTRUCTOR: <mgeneric> mgeneric ( name signature -- generic ) ;
: parse-mgeneric ( -- generic )
    token parse-signature(--) <mgeneric> ;

TUPLE: hook < parsed name symbol signature ;
CONSTRUCTOR: <hook> hook ( name symbol signature -- hook ) ;
: parse-hook ( -- hook )
    token token parse-signature(--) <hook> ;

TUPLE: mgeneric# < parsed name n signature ;
CONSTRUCTOR: <mgeneric#> mgeneric# ( name n signature -- generic ) ;
: parse-mgeneric# ( -- generic )
    token token parse-signature(--) <mgeneric#> ;

TUPLE: mmethod < parsed class name body ;
CONSTRUCTOR: <mmethod> mmethod ( class name body -- method ) ;
: parse-mmethod ( -- method )
    parse token body <mmethod> ;

TUPLE: locals-mmethod < parsed class name body ;
CONSTRUCTOR: <locals-mmethod> locals-mmethod ( class name body -- locals-method ) ;
: parse-locals-mmethod ( -- method )
    parse token body <locals-mmethod> ;

TUPLE: constructor < parsed name class ;
CONSTRUCTOR: <constructor> constructor ( name class -- constructor ) ;
: parse-constructor ( -- constructor )
    token token <constructor> ;

TUPLE: private < parsed body ;
CONSTRUCTOR: <private> private ( body -- private ) ;
: parse-private ( -- private )
    "PRIVATE>" parse-until <private> ;

TUPLE: from < parsed module functions ;
CONSTRUCTOR: <from> from ( module functions -- from ) ;
: parse-from ( -- from )
    token ";" strings-until <from> ;

TUPLE: qualified < parsed name ;
CONSTRUCTOR: <qualified> qualified ( name -- qualified ) ;
: parse-qualified ( -- qualified )
    token <qualified> ;

TUPLE: qualified-with < parsed name prefix ;
CONSTRUCTOR: <qualified-with> qualified-with ( name prefix -- qualified-with ) ;
: parse-qualified-with ( -- qualified-with )
    token token <qualified-with> ;

TUPLE: constant < parsed name object ;
CONSTRUCTOR: <constant> constant ( name object -- constant ) ;
: parse-constant ( -- constant )
    token parse <constant> ;

TUPLE: mtuple < parsed name body ;
CONSTRUCTOR: <mtuple> mtuple ( name body -- tuple ) ;
: parse-mtuple ( -- mtuple )
    token body <mtuple> ;

TUPLE: mbuiltin < parsed name body ;
CONSTRUCTOR: <mbuiltin> mbuiltin ( name body -- builtin ) ;
: parse-mbuiltin ( -- mbuiltin )
    token body <mbuiltin> ;

TUPLE: merror < parsed name body ;
CONSTRUCTOR: <merror> merror ( name body -- error ) ;
: parse-merror ( -- merror )
    token body <merror> ;



TUPLE: mprimitive < parsed name signature ;
CONSTRUCTOR: <mprimitive> mprimitive ( name signature -- package ) ;
: parse-mprimitive ( -- mprimitive )
    parse parse-signature(--) <mprimitive> ;

TUPLE: package < parsed name ;
CONSTRUCTOR: <package> package ( name -- package ) ;
: parse-package ( -- package )
    get-string <package> ;

TUPLE: import < parsed name ;
CONSTRUCTOR: <import> import ( name -- package ) ;
: parse-import ( -- import )
    token <import> ;

TUPLE: imports < parsed names ;
CONSTRUCTOR: <imports> imports ( names -- package ) ;
: parse-imports ( -- import )
    ";" strings-until <imports> ;

TUPLE: mfoldable < parsed ;
CONSTRUCTOR: <mfoldable> mfoldable ( -- obj ) ;
: parse-foldable ( -- mfoldable ) <mfoldable> ;

TUPLE: minline < parsed ;
CONSTRUCTOR: <minline> minline ( -- obj ) ;
: parse-inline ( -- minline ) <minline> ;

TUPLE: mfinal < parsed ;
CONSTRUCTOR: <mfinal> mfinal ( -- obj ) ;
: parse-final ( -- mfinal ) <mfinal> ;

TUPLE: mrecursive < parsed ;
CONSTRUCTOR: <mrecursive> mrecursive ( -- obj ) ;
: parse-recursive ( -- mrecursive ) <mrecursive> ;

TUPLE: munion < parsed name strings ;
CONSTRUCTOR: <munion> munion ( name strings -- obj ) ;
: parse-union ( -- recursive )
    token body <munion> ;

TUPLE: mflushable < parsed ;
CONSTRUCTOR: <mflushable> mflushable ( -- obj ) ;
: parse-flushable ( -- mflushable ) <mflushable> ;

TUPLE: math < parsed name body ;
CONSTRUCTOR: <math> math ( name body -- obj ) ;
: parse-math ( -- builtin )
    token parse-signature(--) <math> ;

TUPLE: functor < parsed name signature definitions ;
CONSTRUCTOR: <functor> functor ( name signature definitions -- functor ) ;
: parse-functor ( -- functor )
    token parse-signature(--) ";FUNCTOR" parse-until <functor> ;

TUPLE: functor-syntax < parsed name body ;
CONSTRUCTOR: <functor-syntax> functor-syntax ( name body -- functor ) ;
: parse-functor-syntax ( -- functor )
    token body <functor-syntax> ;

TUPLE: name < parsed name target ;
CONSTRUCTOR: <name> name ( name target -- object ) ;
: parse-name ( -- name )
    token token <name> ;

TUPLE: defer < parsed name ;
CONSTRUCTOR: <defer> defer ( name -- defer ) ;
: parse-defer ( -- defer )
    token <defer> ;

TUPLE: symbol < parsed name ;
CONSTRUCTOR: <symbol> symbol ( name -- symbol ) ;
: parse-symbol ( -- symbol )
    token <symbol> ;

TUPLE: symbols < parsed names ;
CONSTRUCTOR: <symbols> symbols ( names -- symbols ) ;
: parse-symbols ( -- symbols )
    ";" strings-until <symbols> ;

TUPLE: compilation-unit < parsed code ;
CONSTRUCTOR: <compilation-unit> compilation-unit ( code -- compilation-unit ) ;
: parse-compilation-unit ( -- compilation-unit )
    ">>" parse-until <compilation-unit> ;

TUPLE: rename < parsed function module name ;
CONSTRUCTOR: <rename> rename ( function module name -- rename ) ;
: parse-rename ( -- renamed )
    token token "=>" expect token <rename> ;

TUPLE: typedef < parsed old new ;
CONSTRUCTOR: <typedef> typedef ( old new -- typedef ) ;
: parse-typedef ( -- typedef )
    token token <typedef> ;

TUPLE: library < parsed name ;
CONSTRUCTOR: <library> library ( name -- library ) ;
: parse-library ( -- library )
    token <library> ;

TUPLE: c-function < parsed return-value name arguments ;
CONSTRUCTOR: <c-function> c-function ( return-value name arguments -- c-function ) ;
: parse-c-function ( -- c-function )
    token token c-arguments ";" expect <c-function> ;

TUPLE: x-function < parsed return-value name arguments ;
CONSTRUCTOR: <x-function> x-function ( return-value name arguments -- c-function ) ;
: parse-x-function ( -- c-function )
    token token c-arguments ";" expect <x-function> ;

TUPLE: c-function-alias < parsed aliased-name return-value name arguments ;
CONSTRUCTOR: <c-function-alias> c-function-alias ( aliased-name return-value name arguments -- c-function ) ;
: parse-c-function-alias ( -- c-function )
    token token token c-arguments ";" expect <c-function-alias> ;

TUPLE: gl-function < parsed return-value name ptrs arguments ;
CONSTRUCTOR: <gl-function> gl-function ( return-value name ptrs arguments -- gl-function ) ;
: parse-gl-function ( -- gl-function )
    token token parse c-arguments ";" expect <gl-function> ;


TUPLE: subroutine < parsed name arguments ;
CONSTRUCTOR: <subroutine> subroutine ( name arguments -- subroutine ) ;
: parse-subroutine ( -- subroutine )
    token c-arguments ";" expect <subroutine> ;
\ parse-subroutine "SUBROUTINE:" register-parser

TUPLE: c-type < parsed name ;
CONSTRUCTOR: <c-type> c-type ( name -- c-type ) ;
: parse-c-type ( -- c-type )
    token <c-type> ;

TUPLE: mmacro < parsed name signature body ;
CONSTRUCTOR: <mmacro> mmacro ( name signature body -- macro ) ;
: parse-macro ( -- macro )
    token parse-signature(--) ";" parse-until <mmacro> ;

TUPLE: locals-macro < parsed name signature body ;
CONSTRUCTOR: <locals-macro> locals-macro ( name signature body -- macro ) ;
: parse-locals-macro ( -- macro )
    token parse-signature(--) ";" parse-until <locals-macro> ;

TUPLE: struct < parsed name slots ;
CONSTRUCTOR: <struct> struct ( name slots -- struct ) ;
: parse-struct ( -- struct )
    token ";" parse-until <struct> ;

TUPLE: packed-struct < parsed name slots ;
CONSTRUCTOR: <packed-struct> packed-struct ( name slots -- packed-struct ) ;
: parse-packed-struct ( -- struct )
    token ";" parse-until <packed-struct> ;

TUPLE: union-struct < parsed name slots ;
CONSTRUCTOR: <union-struct> union-struct ( name slots -- union-struct ) ;
: parse-union-struct ( -- union-struct )
    token ";" parse-until <union-struct> ;

TUPLE: alias < parsed name target ;
CONSTRUCTOR: <alias> alias ( name target -- alias ) ;
: parse-alias ( -- alias )
    token token <alias> ;

TUPLE: mregisters < parsed names ;
CONSTRUCTOR: <mregisters> mregisters ( names -- obj ) ;
: parse-registers ( -- obj )
    ";" parse-until <mregisters> ;

TUPLE: mhi-registers < parsed names ;
CONSTRUCTOR: <mhi-registers> mhi-registers ( names -- obj ) ;
: parse-hi-registers ( -- obj )
    ";" parse-until <mhi-registers> ;

TUPLE: about < parsed name ;
CONSTRUCTOR: <about> about ( name -- obj ) ;
: parse-about ( -- obj )
    token <about> ;

TUPLE: article < parsed name body ;
CONSTRUCTOR: <article> article ( name body -- obj ) ;
: parse-article ( -- obj )
    token ";" parse-until <article> ;

TUPLE: c-global < parsed type name ;
CONSTRUCTOR: <c-global> c-global ( type name -- obj ) ;
: parse-c-global ( -- obj )
    token token <c-global> ;

TUPLE: protocol < parsed name functions ;
CONSTRUCTOR: <protocol> protocol ( name functions -- obj ) ;
: parse-protocol ( -- obj )
    token ";" parse-until <protocol> ;

TUPLE: exclude < parsed name body ;
CONSTRUCTOR: <exclude> exclude ( name body -- obj ) ;
: parse-exclude ( -- obj )
    token "=>" expect ";" parse-until <exclude> ;

TUPLE: mfoldable-insn < parsed name body ;
CONSTRUCTOR: <mfoldable-insn> mfoldable-insn ( name body -- obj ) ;
: parse-foldable-insn ( -- obj )
    token ";" parse-until <mfoldable-insn> ;

TUPLE: mflushable-insn < parsed name body ;
CONSTRUCTOR: <mflushable-insn> mflushable-insn ( name body -- obj ) ;
: parse-flushable-insn ( -- obj )
    token ";" parse-until <mflushable-insn> ;

TUPLE: mvreg-insn < parsed name body ;
CONSTRUCTOR: <mvreg-insn> mvreg-insn ( name body -- obj ) ;
: parse-vreg-insn ( -- obj )
    token ";" parse-until <mvreg-insn> ;

TUPLE: minsn < parsed name body ;
CONSTRUCTOR: <minsn> minsn ( name body -- obj ) ;
: parse-insn ( -- obj )
    token ";" parse-until <minsn> ;

TUPLE: codegen < parsed name1 name2 ;
CONSTRUCTOR: <codegen> codegen ( name1 name2 -- obj ) ;
: parse-codegen ( -- obj )
    token token <codegen> ;

TUPLE: conditional < parsed name1 name2 ;
CONSTRUCTOR: <conditional> conditional ( name1 name2 -- obj ) ;
: parse-conditional ( -- obj )
    token token <conditional> ;

TUPLE: simd-128 < parsed name ;
CONSTRUCTOR: <simd-128> simd-128 ( name -- obj ) ;
: parse-simd-128 ( -- obj )
    token <simd-128> ;

TUPLE: simd-128-cord < parsed name1 name2 ;
CONSTRUCTOR: <simd-128-cord> simd-128-cord ( name1 name2 -- obj ) ;
: parse-simd-128-cord ( -- obj )
    token token <simd-128-cord> ;

TUPLE: simd-intrinsic < parsed name body ;
CONSTRUCTOR: <simd-intrinsic> simd-intrinsic ( name body -- obj ) ;
: parse-simd-intrinsic ( -- obj )
    token ";" parse-until <simd-intrinsic> ;

TUPLE: locals-simd-intrinsic < parsed name body ;
CONSTRUCTOR: <locals-simd-intrinsic> locals-simd-intrinsic ( name body -- obj ) ;
: parse-locals-simd-intrinsic ( -- obj )
    token ";" parse-until <locals-simd-intrinsic> ;

TUPLE: menum < parsed name slots ;
CONSTRUCTOR: <menum> menum ( name slots -- obj ) ;
: parse-enum ( -- obj )
    token ";" parse-until <menum> ;

TUPLE: forget < parsed name ;
CONSTRUCTOR: <forget> forget ( name -- obj ) ;
: parse-forget ( -- obj )
    token <forget> ;

TUPLE: mpointer < parsed to ;
CONSTRUCTOR: <mpointer> mpointer ( to -- obj ) ;
: parse-pointer ( -- obj )
    token <mpointer> ;

TUPLE: help < parsed name body ;
CONSTRUCTOR: <help> help ( name body -- obj ) ;
: parse-help ( -- help )
    token
    ";" parse-until <help> ;

TUPLE: long-string < parsed name text ;
CONSTRUCTOR: <long-string> long-string ( name text -- long-string ) ;
: parse-long-string ( -- long-string )
    token "\n;" multiline-string-until <long-string> ;

TUPLE: mirc < parsed name command body ;
CONSTRUCTOR: <mirc> mirc ( name command body -- mirc ) ;
: parse-irc ( -- irc )
    token parse ";" strings-until <mirc> ;

\ parse-parser "PARSER:" register-parser
\ parse-package "PACKAGE:" register-parser
\ parse-import "IMPORT:" register-parser
\ parse-imports "IMPORTS:" register-parser
\ parse-author "AUTHOR:" register-parser
\ parse-from "FROM:" register-parser
\ parse-qualified "QUALIFIED:" register-parser
\ parse-qualified-with "QUALIFIED-WITH:" register-parser
\ parse-use "USE:" register-parser
\ parse-using "USING:" register-parser
\ parse-in "IN:" register-parser
\ parse-main "MAIN:" register-parser
\ parse-math "MATH:" register-parser
\ parse-union "UNION:" register-parser
\ parse-char "CHAR:" register-parser
\ parse-escaped "\\" register-parser
\ parse-execute( "execute(" register-parser
\ parse-call( "call(" register-parser
\ parse-data-map( "data-map(" register-parser
\ parse-data-map!( "data-map!(" register-parser
\ parse-private "<PRIVATE" register-parser
\ parse-constant "CONSTANT:" register-parser
\ parse-mtuple "TUPLE:" register-parser
\ parse-mbuiltin "BUILTIN:" register-parser
\ parse-merror "ERROR:" register-parser
\ parse-mprimitive "PRIMITIVE:" register-parser
\ parse-foldable "foldable" register-parser
\ parse-flushable "flushable" register-parser
\ parse-inline "inline" register-parser
\ parse-final "final" register-parser
\ parse-recursive "recursive" register-parser
\ parse-block "[" register-parser
\ parse-parsetime-block "$[" register-parser
\ parse-locals-block "[|" register-parser
\ parse-bind ":>" register-parser
\ parse-fry "'[" register-parser
\ parse-marray "{" register-parser
\ parse-mvector "V{" register-parser
\ parse-mhashtable "H{" register-parser
\ parse-tuple-literal "T{" register-parser
\ parse-mgeneric "GENERIC:" register-parser
\ parse-mgeneric# "GENERIC#" register-parser
\ parse-mmethod "M:" register-parser
\ parse-locals-mmethod "M::" register-parser
\ parse-constructor "C:" register-parser
\ parse-function ":" register-parser
\ parse-locals-function "::" register-parser
\ parse-typed "TYPED:" register-parser
\ parse-locals-typed "TYPED::" register-parser
\ parse-memo "MEMO:" register-parser
\ parse-locals-memo "MEMO::" register-parser
\ parse-instance "INSTANCE:" register-parser
\ parse-predicate "PREDICATE:" register-parser
\ parse-slot "SLOT:" register-parser
\ parse-mixin "MIXIN:" register-parser
\ parse-singleton "SINGLETON:" register-parser
\ parse-singletons "SINGLETONS:" register-parser
\ parse-comment "!" register-parser
\ parse-comment "#!" register-parser
\ parse-nested-comment "(*" register-parser
\ parse-postpone "POSTPONE:" register-parser
\ parse-hints "HINTS:" register-parser
\ parse-specialized-array "SPECIALIZED-ARRAY:" register-parser
\ parse-specialized-arrays "SPECIALIZED-ARRAYS:" register-parser
\ parse-syntax "SYNTAX:" register-parser
\ parse-functor "FUNCTOR:" register-parser
\ parse-functor-syntax "FUNCTOR-SYNTAX:" register-parser
\ parse-name "NAME:" register-parser
\ parse-symbol "SYMBOL:" register-parser
\ parse-symbols "SYMBOLS:" register-parser
\ parse-defer "DEFER:" register-parser
\ parse-hook "HOOK:" register-parser
\ parse-compilation-unit "<<" register-parser
\ parse-rename "RENAME:" register-parser
\ parse-typedef "TYPEDEF:" register-parser
\ parse-c-function "FUNCTION:" register-parser
\ parse-x-function "X-FUNCTION:" register-parser
\ parse-c-function-alias "FUNCTION-ALIAS:" register-parser
\ parse-gl-function "GL-FUNCTION:" register-parser
\ parse-library "LIBRARY:" register-parser
\ parse-c-type "C-TYPE:" register-parser
\ parse-macro "MACRO:" register-parser
\ parse-locals-macro "MACRO::" register-parser
\ parse-struct "STRUCT:" register-parser
\ parse-packed-struct "PACKED-STRUCT:" register-parser
\ parse-union-struct "UNION-STRUCT:" register-parser
\ parse-alias "ALIAS:" register-parser
\ parse-registers "REGISTERS:" register-parser
\ parse-hi-registers "HI-REGISTERS:" register-parser
\ parse-about "ABOUT:" register-parser
\ parse-c-global "C-GLOBAL:" register-parser
\ parse-article "ARTICLE:" register-parser
\ parse-protocol "PROTOCOL:" register-parser
\ parse-exclude "EXCLUDE:" register-parser
\ parse-foldable-insn "FOLDABLE-INSN:" register-parser
\ parse-flushable-insn "FLUSHABLE-INSN:" register-parser
\ parse-insn "INSN:" register-parser
\ parse-vreg-insn "VREG-INSN:" register-parser
\ parse-codegen "CODEGEN:" register-parser
\ parse-conditional "CONDITIONAL:" register-parser
\ parse-simd-128 "SIMD-128:" register-parser
\ parse-simd-128-cord "SIMD-128-CORD:" register-parser
\ parse-simd-intrinsic "SIMD-INTRINSIC:" register-parser
\ parse-locals-simd-intrinsic "SIMD-INTRINSIC::" register-parser
\ parse-enum "ENUM:" register-parser
\ parse-forget "FORGET:" register-parser
\ parse-pointer "pointer:" register-parser
\ parse-long-string "STRING:" register-parser
\ parse-help "HELP:" register-parser
\ parse-irc "IRC:" register-parser

! The above is enough to kind of correctly parse factor.

! tools.test
TUPLE: mtest < parsed name ;
CONSTRUCTOR: <mtest> mtest ( name -- obj ) ;
: parse-test ( -- obj )
    token <mtest> ;
\ parse-test "TEST:" register-parser

TUPLE: mspecial-object < parsed name value ;
CONSTRUCTOR: <mspecial-object> mspecial-object ( name value -- obj ) ;
: parse-special-object ( -- obj )
    token parse <mspecial-object> ;
\ parse-special-object "SPECIAL-OBJECT:" register-parser

TUPLE: mreset < parsed name value ;
CONSTRUCTOR: <mreset> mreset ( -- obj ) ;
: parse-reset ( -- obj )
    <mreset> ;
\ parse-reset "RESET" register-parser


TUPLE: tr < parsed name body ;
CONSTRUCTOR: <tr> tr ( name body -- obj ) ;
: parse-tr ( -- obj )
    token ";" parse-until <tr> ;

\ parse-tr "TR:" register-parser


TUPLE: mintersection < parsed name body ;
CONSTRUCTOR: <mintersection> mintersection ( name body -- obj ) ;
: parse-intersection ( -- obj )
    token ";" parse-until <mintersection> ;
\ parse-intersection "INTERSECTION:" register-parser

TUPLE: new-constructor < parsed name class signature body ;
CONSTRUCTOR: <new-constructor> new-constructor ( name class signature body -- obj ) ;
: parse-new-constructor ( -- obj )
    token token parse-signature(--) ";" parse-until <new-constructor> ;
\ parse-new-constructor "CONSTRUCTOR:" register-parser

TUPLE: method-literal < parsed class generic ;
CONSTRUCTOR: <method-literal> method-literal ( class generic -- obj ) ;
: parse-method-literal ( -- obj )
    token token <method-literal> ;
\ parse-method-literal "M\\" register-parser

TUPLE: unicode-category < parsed name body ;
CONSTRUCTOR: <unicode-category> unicode-category ( name body -- obj ) ;
: parse-unicode-category ( -- obj )
    token token <unicode-category> ;
\ parse-unicode-category "CATEGORY:" register-parser

TUPLE: unicode-category-not < parsed name body ;
CONSTRUCTOR: <unicode-category-not> unicode-category-not ( name body -- obj ) ;
: parse-unicode-category-not ( -- obj )
    token token <unicode-category-not> ;
\ parse-unicode-category-not "CATEGORY-NOT:" register-parser

TUPLE: main-window < parsed name attributes body ;
CONSTRUCTOR: <main-window> main-window ( name attributes body -- obj ) ;
: parse-main-window ( -- obj )
    token parse body <main-window> ;
\ parse-main-window "MAIN-WINDOW:" register-parser

TUPLE: game < parsed name attributes body ;
CONSTRUCTOR: <game> game ( name attributes body -- obj ) ;
: parse-game ( -- obj )
    token parse body <game> ;
\ parse-game "GAME:" register-parser

TUPLE: solution < parsed name ;
CONSTRUCTOR: <solution> solution ( name -- obj ) ;
: parse-solution ( -- obj )
    token <solution> ;
\ parse-solution "SOLUTION:" register-parser

TUPLE: c-callback < parsed return-value name arguments ;
CONSTRUCTOR: <c-callback> c-callback ( return-value name arguments -- c-callback ) ;
: parse-c-callback ( -- c-callback )
    token token c-arguments ";" expect <c-callback> ;
\ parse-c-callback "CALLBACK:" register-parser


TUPLE: 8-bit < parsed name encoding1 encoding2 ;
CONSTRUCTOR: <8-bit> 8-bit ( name encoding1 encoding2 -- 8-bit ) ;
: parse-8-bit ( -- 8-bit )
    token token token <8-bit> ;
\ parse-8-bit "8-BIT:" register-parser


TUPLE: munit-test < parsed ;
CONSTRUCTOR: <munit-test> munit-test ( -- obj ) ;
: parse-unit-test ( -- munit-test ) <munit-test> ;
\ parse-unit-test "unit-test" register-parser


TUPLE: slots-quot < parsed body ;
CONSTRUCTOR: <slots-quot> slots-quot ( body -- block ) ;
: parse-slots-quot ( -- block )
    "]" parse-until <slots-quot> ;
\ parse-slots-quot "slots[" register-parser

TUPLE: slots-array < parsed body ;
CONSTRUCTOR: <slots-array> slots-array ( body -- obj ) ;
: parse-slots-array ( -- array )
    "}" parse-until <slots-array> ;
\ parse-slots-array "slots{" register-parser

TUPLE: set-slots-array < parsed body ;
CONSTRUCTOR: <set-slots-array> set-slots-array ( body -- obj ) ;
: parse-set-slots-array ( -- array )
    "}" parse-until <set-slots-array> ;
\ parse-set-slots-array "set-slots{" register-parser

TUPLE: set-slots-quot < parsed body ;
CONSTRUCTOR: <set-slots-quot> set-slots-quot ( body -- obj ) ;
: parse-set-slots-quot ( -- obj )
    "]" parse-until <set-slots-quot> ;
\ parse-set-slots-quot "set-slots[" register-parser

TUPLE: copy-slots-array < parsed body ;
CONSTRUCTOR: <copy-slots-array> copy-slots-array ( body -- obj ) ;
: parse-copy-slots-array ( -- array )
    "}" parse-until <copy-slots-array> ;
\ parse-copy-slots-array "copy-slots{" register-parser

TUPLE: set-array < parsed body ;
CONSTRUCTOR: <set-array> set-array ( body -- obj ) ;
: parse-set-array ( -- obj )
    "}" parse-until <set-array> ;
\ parse-set-array "set{" register-parser

TUPLE: set-quot < parsed body ;
CONSTRUCTOR: <set-quot> set-quot ( body -- obj ) ;
: parse-set-quot ( -- obj )
    "}" parse-until <set-quot> ;
\ parse-set-quot "set[" register-parser

TUPLE: get-array < parsed body ;
CONSTRUCTOR: <get-array> get-array ( body -- obj ) ;
: parse-get-array ( -- obj )
    "}" parse-until <get-array> ;
\ parse-get-array "get{" register-parser

TUPLE: get-quot < parsed body ;
CONSTRUCTOR: <get-quot> get-quot ( body -- obj ) ;
: parse-get-quot ( -- obj )
    "}" parse-until <get-quot> ;
\ parse-get-quot "get[" register-parser



TUPLE: let-block < parsed body ;
CONSTRUCTOR: <let-block> let-block ( body -- block ) ;
: parse-let-block ( -- let-block )
    "]" parse-until <let-block> ;
\ parse-let-block "[let" register-parser

TUPLE: stack-effect < parsed in out ;
CONSTRUCTOR: <stack-effect> stack-effect ( in out -- obj ) ;
: parse-stack-effect ( -- stack-effect )
    parse-signature--) ;
\ parse-stack-effect "(" register-parser


TUPLE: heredoc < parsed name string ;
CONSTRUCTOR: <heredoc> heredoc ( name string -- heredoc ) ;
: parse-heredoc ( -- heredoc )
    token
    dup name>> multiline-string-until <heredoc> ;
\ parse-heredoc "HEREDOC:" register-parser

TUPLE: color < parsed name ;
CONSTRUCTOR: <color> color ( name -- color ) ;
: parse-color ( -- color )
    token <color> ;
\ parse-color "COLOR:" register-parser


TUPLE: ebnf < parsed name text ;
CONSTRUCTOR: <ebnf> ebnf ( name text -- ebnf ) ;
: parse-ebnf ( -- ebnf )
    token
    ";EBNF" multiline-string-until <ebnf> ;
\ parse-ebnf "EBNF:" register-parser

: parse-ebnf-acute ( -- ebnf )
    token
    "EBNF>" multiline-string-until <ebnf> ;
\ parse-ebnf-acute "<EBNF" register-parser

: parse-ebnf-bracket ( -- ebnf-bracket )
    token
    "EBNF]" multiline-string-until <ebnf> ;
\ parse-ebnf-bracket "[EBNF" register-parser


TUPLE: glsl-shader < parsed name class string ;
CONSTRUCTOR: <glsl-shader> glsl-shader ( name class string -- glsl ) ;
: parse-glsl-shader ( -- heredoc )
    token token "\n;" multiline-string-until <glsl-shader> ;
\ parse-glsl-shader "GLSL-SHADER:" register-parser

TUPLE: glsl-program < parsed name words ;
CONSTRUCTOR: <glsl-program> glsl-program ( name words -- glsl ) ;
: parse-glsl-program ( -- heredoc )
    token ";" parse-until <glsl-program> ;
\ parse-glsl-program "GLSL-PROGRAM:" register-parser

TUPLE: uniform-tuple < parsed name slots ;
CONSTRUCTOR: <uniform-tuple> uniform-tuple ( name slots -- uniform-tuple ) ;
: parse-uniform-tuple ( -- uniform-tuple )
    token ";" parse-until <uniform-tuple> ;
\ parse-uniform-tuple "UNIFORM-TUPLE:" register-parser


/*
all-words [ "syntax" word-prop ] filter
[ vocabulary>> ] collect-by >alist
[ first2 [ [ ".private" ?tail drop modern-source-path ] keep ] dip 3array ] map
sort-keys [ . ] each
{
    "resource:basis/alien/data/data.factor"
    "alien.data"
    V{ POSTPONE: c-array{ POSTPONE: c-array@ }
} .......

find . | grep '\-syntax.modern' | xargs cat
*/

(*
all-words [ "syntax" word-prop ] filter
[ vocabulary>> ] collect-by .


! words we define that aren't parsing words
load-all
parsers get-global keys
all-words [ parsing-word? ] filter
natural-sort [ name>> ] map diff .

! words we don't define
! not generated words
parsers get-global keys
all-words [ "syntax" word-prop ] filter
[ name>> ] map swap diff
natural-sort
[ . ] each

"\"" "$" "${" "&:" "(" "->" "/*" "8-BIT:"
"<<<<<<" "<<<<<<<" "======" "=======" ">>>>>>" ">>>>>>>"
"?{" "ALIEN:" "B" "B:" "BACKWARD-ANALYSIS:" "BAD-ALIEN" "BROADCAST:" "BV{"
"B{" "CATEGORY-NOT:" "CATEGORY:" "CFSTRING:" "CLASS:" "CONSTRUCTOR:"
"CONSULT:" "CS{" "C{" "D" "DEFERS" "DEFINES" "DEFINES-CLASS" "DEFINES-PRIVATE"
"DELIMITED:" "DESTRUCTOR:" "DLL\"" "DL{" "FORWARD-ANALYSIS:" "FRAMEWORK:" "HS{"
"ICON:" "IDENTITY-MEMO:" "IDENTITY-MEMO::" "IHS{" "IH{" "INTERSECTION:" "IS" "I["
"LOG:" "MAIN-WINDOW:" "METHOD:" "M\\" "NAN:" "P\"" "PEG:"
"PIXEL-FORMAT-ATTRIBUTE-TABLE:" "PRIVATE>" "R" "R!" "R\"" "R#" "R'" "R(" "R/" "R@"
"RECT:" "RENAMING:" "RESET" "R[" "R`" "R{" "R|" "S@" "SBUF\"" "SEL:" "SLOT-CONSTRUCTOR:"
"SLOT-PROTOCOL:" "SPECIAL-OBJECT:" "SUPER->" "S{" "TEST:" "TIP:" "TOKENIZER:" "TR:"
"UNION-STRUCT:" "UNUSE:" "URL\"" "W{" "X509_V_:" "c-array@" "c-array{" "call-next-method"
"delimiter" "deprecated" "eval(" "f" "flags{" "intersection{" "maybe{" "not{"
"shuffle(" "union{"

*)
