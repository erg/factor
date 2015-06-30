! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs combinators constructors kernel make
modern.parser multiline namespaces nested-comments sequences ;
IN: modern.parser.factor


TUPLE: mparser < parsed name start slots body ;
CONSTRUCTOR: <mparser> mparser ( name slots start body -- mparser ) ;
: parse-parser ( -- mparser )
    raw parse raw body <mparser> ;

ERROR: string-expected got separator ;
! TUPLE: mstring < parsed class string ;
! CONSTRUCTOR: <mstring> mstring ( class string -- mstring ) ;

TUPLE: text < parsed string from to ;
CONSTRUCTOR: <text> text ( string from to -- text ) ;

! TUPLE: comment < parsed text ;
! CONSTRUCTOR: <comment> comment ( text -- comment ) ;

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
    token body <hints> ;

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

TUPLE: ebnf < parsed name text ;
CONSTRUCTOR: <ebnf> ebnf ( name text -- ebnf ) ;
: parse-ebnf ( -- ebnf )
    token
    ";EBNF" multiline-string-until <ebnf> ;

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
    ";" parse-until <symbols> ;

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
    token token ";" parse-until <c-function> ;

TUPLE: x-function < parsed return-value name arguments ;
CONSTRUCTOR: <x-function> x-function ( return-value name arguments -- c-function ) ;
: parse-x-function ( -- c-function )
    token token ";" parse-until <x-function> ;

TUPLE: c-function-alias < parsed aliased-name return-value name arguments ;
CONSTRUCTOR: <c-function-alias> c-function-alias ( aliased-name return-value name arguments -- c-function ) ;
: parse-c-function-alias ( -- c-function )
    token token token ";" parse-until <c-function-alias> ;

TUPLE: gl-function < parsed return-value name arguments ;
CONSTRUCTOR: <gl-function> gl-function ( return-value name arguments -- gl-function ) ;
: parse-gl-function ( -- gl-function )
    token token ";" parse-until <gl-function> ;

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
CONSTRUCTOR: <packed-struct> packed-struct ( name slots -- struct ) ;
: parse-packed-struct ( -- struct )
    token ";" parse-until <packed-struct> ;

TUPLE: alias < parsed name target ;
CONSTRUCTOR: <alias> alias ( name target -- alias ) ;
: parse-alias ( -- alias )
    token parse <alias> ;

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

TUPLE: tr < parsed name body ;
CONSTRUCTOR: <tr> tr ( name body -- obj ) ;
: parse-tr ( -- obj )
    token ";" parse-until <tr> ;

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
    token ";" parse-comment-until <long-string> ;

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
\ parse-ebnf "EBNF:" register-parser
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

/*
all-words [ "syntax" word-prop ] filter
[ vocabulary>> ] collect-by >alist
[ first2 [ [ ".private" ?tail drop modern-source-path ] keep ] dip 3array ] map
sort-keys [ . ] each
{
    "resource:basis/alien/data/data.factor"
    "alien.data"
    V{ POSTPONE: c-array{ POSTPONE: c-array@ }
}
{
    "resource:basis/alien/destructors/destructors.modern"
    "alien.destructors"
    V{ POSTPONE: DESTRUCTOR: }
}
{
    "resource:basis/alien/endian/endian.factor"
    "alien.endian"
    V{
        POSTPONE: LE-PACKED-STRUCT:
        POSTPONE: LE-STRUCT:
        POSTPONE: BE-PACKED-STRUCT:
        POSTPONE: BE-STRUCT:
    }
}
{
    "resource:basis/alien/syntax/syntax.factor"
    "alien.syntax"
    V{
        POSTPONE: &:
        POSTPONE: CALLBACK:
        POSTPONE: FUNCTION:
        POSTPONE: ALIEN:
        POSTPONE: TYPEDEF:
        POSTPONE: DLL"
        POSTPONE: pointer:
        POSTPONE: ENUM:
        POSTPONE: C-TYPE:
        POSTPONE: BAD-ALIEN
        POSTPONE: FUNCTION-ALIAS:
        POSTPONE: C-GLOBAL:
        POSTPONE: LIBRARY:
    }
}
{
    "resource:basis/bit-arrays/bit-arrays.factor"
    "bit-arrays"
    V{ POSTPONE: ?{ }
}
{
    "resource:basis/bit-vectors/bit-vectors.factor"
    "bit-vectors"
    V{ POSTPONE: ?V{ }
}
{
    "resource:basis/bootstrap/image/syntax/syntax.factor"
    "bootstrap.image.syntax"
    V{ POSTPONE: RESET POSTPONE: SPECIAL-OBJECT: }
}
{
    "resource:basis/byte-arrays/hex/hex.factor"
    "byte-arrays.hex"
    V{ POSTPONE: HEX{ }
}
{
    "resource:basis/classes/struct/struct.factor"
    "classes.struct"
    V{
        POSTPONE: UNION-STRUCT:
        POSTPONE: STRUCT:
        POSTPONE: PACKED-STRUCT:
        POSTPONE: S@
        POSTPONE: S{
    }
}
{
    "resource:basis/cocoa/apple-script/apple-script.factor"
    "cocoa.apple-script"
    V{ POSTPONE: APPLESCRIPT: }
}
{
    "resource:basis/cocoa/cocoa.factor"
    "cocoa"
    V{
        POSTPONE: ->
        POSTPONE: IMPORT:
        POSTPONE: SEL:
        POSTPONE: FRAMEWORK:
        POSTPONE: SUPER->
    }
}
{
    "resource:basis/cocoa/subclassing/subclassing.factor"
    "cocoa.subclassing"
    V{ POSTPONE: CLASS: POSTPONE: METHOD: }
}
{
    "resource:basis/colors/constants/constants.factor"
    "colors.constants"
    V{ POSTPONE: COLOR: }
}
{
    "resource:basis/colors/hex/hex.factor"
    "colors.hex"
    V{ POSTPONE: HEXCOLOR: }
}
{
    "resource:basis/compiler/cfg/dataflow-analysis/dataflow-analys..."
    "compiler.cfg.dataflow-analysis"
    V{
        POSTPONE: FORWARD-ANALYSIS:
        POSTPONE: BACKWARD-ANALYSIS:
    }
}
{
    "resource:basis/compiler/cfg/instructions/syntax/syntax.factor"
    "compiler.cfg.instructions.syntax"
    V{
        POSTPONE: INSN:
        POSTPONE: VREG-INSN:
        POSTPONE: FLUSHABLE-INSN:
        POSTPONE: FOLDABLE-INSN:
    }
}
{
    "resource:basis/compiler/cfg/registers/registers.factor"
    "compiler.cfg.registers"
    V{ POSTPONE: D POSTPONE: R }
}
{
    "resource:basis/compiler/cfg/renaming/functor/functor.modern"
    "compiler.cfg.renaming.functor"
    V{ POSTPONE: RENAMING: }
}
{
    "resource:basis/compiler/codegen/codegen.factor"
    "compiler.codegen"
    V{ POSTPONE: CONDITIONAL: POSTPONE: CODEGEN: }
}
{
    "resource:basis/core-foundation/strings/strings.factor"
    "core-foundation.strings"
    V{ POSTPONE: CFSTRING: }
}
{
    "resource:basis/cpu/x86/assembler/syntax/syntax.factor"
    "cpu.x86.assembler.syntax"
    V{ POSTPONE: HI-REGISTERS: POSTPONE: REGISTERS: }
}
{
    "resource:basis/definitions/icons/icons.factor"
    "definitions.icons"
    V{ POSTPONE: ICON: }
}
{
    "resource:basis/delegate/delegate.factor"
    "delegate"
    V{
        POSTPONE: PROTOCOL:
        POSTPONE: SLOT-PROTOCOL:
        POSTPONE: BROADCAST:
        POSTPONE: CONSULT:
    }
}
{
    "resource:basis/dlists/dlists.factor"
    "dlists"
    V{ POSTPONE: DL{ }
}
{
    "resource:basis/eval/eval.factor"
    "eval"
    V{ POSTPONE: eval( }
}
{ "resource:basis/fry/fry.factor" "fry" V{ POSTPONE: '[ } }
{
    "resource:basis/functors/backend/backend.factor"
    "functors.backend"
    V{ POSTPONE: FUNCTOR-SYNTAX: }
}
{
    "resource:basis/functors/functors.factor"
    "functors"
    V{
        POSTPONE: FUNCTOR:
        POSTPONE: DEFINES-CLASS
        POSTPONE: DEFERS
        POSTPONE: IS
        POSTPONE: DEFINES
        POSTPONE: DEFINES-PRIVATE
    }
}
{
    "resource:basis/gobject-introspection/gobject-introspection.fa..."
    "gobject-introspection"
    V{
        POSTPONE: IMPLEMENT-STRUCTS:
        POSTPONE: FOREIGN-ENUM-TYPE:
        POSTPONE: GIR:
        POSTPONE: FOREIGN-RECORD-TYPE:
        POSTPONE: FOREIGN-ATOMIC-TYPE:
    }
}
{
    "resource:basis/hash-sets/identity/identity.factor"
    "hash-sets.identity"
    V{ POSTPONE: IHS{ }
}
{
    "resource:basis/hash-sets/sequences/sequences.factor"
    "hash-sets.sequences"
    V{ POSTPONE: SHS{ }
}
{
    "resource:basis/hashtables/identity/identity.factor"
    "hashtables.identity"
    V{ POSTPONE: IH{ }
}
{
    "resource:basis/hashtables/sequences/sequences.factor"
    "hashtables.sequences"
    V{ POSTPONE: SH{ }
}
{
    "resource:basis/help/syntax/syntax.factor"
    "help.syntax"
    V{ POSTPONE: HELP: POSTPONE: ARTICLE: POSTPONE: ABOUT: }
}
{
    "resource:basis/help/tips/tips.factor"
    "help.tips"
    V{ POSTPONE: TIP: }
}
{
    "resource:basis/hints/hints.factor"
    "hints"
    V{ POSTPONE: HINTS: }
}
{
    "resource:basis/html/templates/chloe/components/components.factor"
    "html.templates.chloe.components"
    V{ POSTPONE: COMPONENT: }
}
{
    "resource:basis/html/templates/chloe/syntax/syntax.factor"
    "html.templates.chloe.syntax"
    V{ POSTPONE: CHLOE: }
}
{
    "resource:basis/html/templates/fhtml/fhtml.factor"
    "html.templates.fhtml"
    V{ POSTPONE: %> }
}
{
    "resource:basis/interpolate/interpolate.factor"
    "interpolate"
    V{ POSTPONE: I[ }
}
{
    "resource:basis/io/encodings/8-bit/8-bit.factor"
    "io.encodings.8-bit"
    V{ POSTPONE: 8-BIT: }
}
{
    "resource:basis/io/encodings/euc/euc.factor"
    "io.encodings.euc"
    V{ POSTPONE: EUC: }
}
{
    "resource:basis/io/encodings/iso2022/iso2022.factor"
    "io.encodings.iso2022.private"
    V{ POSTPONE: ESC }
}
{
    "resource:basis/literals/literals.factor"
    "literals"
    V{ POSTPONE: $[ POSTPONE: ${ POSTPONE: $ POSTPONE: flags{ }
}
{
    "resource:basis/locals/locals.factor"
    "locals"
    V{
        POSTPONE: ::
        POSTPONE: M::
        POSTPONE: :>
        POSTPONE: MEMO::
        POSTPONE: [|
        POSTPONE: [let
        POSTPONE: IDENTITY-MEMO::
        POSTPONE: MACRO::
    }
}
{
    "resource:basis/logging/logging.factor"
    "logging"
    V{ POSTPONE: LOG: }
}
{
    "resource:basis/macros/macros.factor"
    "macros"
    V{ POSTPONE: MACRO: }
}
{
    "resource:basis/match/match.factor"
    "match"
    V{ POSTPONE: MATCH-VARS: }
}
{
    "resource:basis/math/rectangles/rectangles.factor"
    "math.rectangles"
    V{ POSTPONE: RECT: }
}
{
    "resource:basis/math/vectors/simd/cords/cords.modern"
    "math.vectors.simd.cords.private"
    V{ POSTPONE: SIMD-128-CORD: }
}
{
    "resource:basis/math/vectors/simd/intrinsics/intrinsics.factor"
    "math.vectors.simd.intrinsics"
    V{ POSTPONE: SIMD-INTRINSIC: POSTPONE: SIMD-INTRINSIC:: }
}
{
    "resource:basis/math/vectors/simd/simd.modern"
    "math.vectors.simd.private"
    V{ POSTPONE: SIMD-128: }
}
{
    "resource:basis/memoize/memoize.factor"
    "memoize"
    V{ POSTPONE: IDENTITY-MEMO: POSTPONE: MEMO: }
}
{
    "resource:basis/memoize/syntax/syntax.factor"
    "memoize.syntax"
    V{ POSTPONE: MEMO[ }
}
{
    "resource:basis/multiline/multiline.factor"
    "multiline"
    V{
        POSTPONE: DELIMITED:
        POSTPONE: STRING:
        POSTPONE: /*
        POSTPONE: HEREDOC:
    }
}
{
    "resource:basis/nibble-arrays/nibble-arrays.factor"
    "nibble-arrays"
    V{ POSTPONE: N{ }
}
{
    "resource:basis/opengl/debug/debug.factor"
    "opengl.debug"
    V{ POSTPONE: GB }
}
{
    "resource:basis/opengl/gl/extensions/extensions.factor"
    "opengl.gl.extensions"
    V{ POSTPONE: GL-FUNCTION: }
}
{
    "resource:basis/openssl/libssl/libssl.factor"
    "openssl.libssl"
    V{ POSTPONE: X509_V_: }
}
{
    "resource:basis/peg/ebnf/ebnf.factor"
    "peg.ebnf"
    V{ POSTPONE: EBNF: POSTPONE: [EBNF POSTPONE: <EBNF }
}
{
    "resource:basis/peg/ebnf/ebnf.factor"
    "peg.ebnf.private"
    V{ POSTPONE: TOKENIZER: }
}
{ "resource:basis/peg/peg.factor" "peg" V{ POSTPONE: PEG: } }
{
    "resource:basis/persistent/hashtables/hashtables.factor"
    "persistent.hashtables"
    V{ POSTPONE: PH{ }
}
{
    "resource:basis/persistent/vectors/vectors.factor"
    "persistent.vectors"
    V{ POSTPONE: PV{ }
}
{
    "resource:basis/promises/promises.factor"
    "promises"
    V{ POSTPONE: LAZY: }
}
{
    "resource:basis/regexp/regexp.factor"
    "regexp"
    V{
        POSTPONE: R|
        POSTPONE: R@
        POSTPONE: R/
        POSTPONE: R{
        POSTPONE: R"
        POSTPONE: R#
        POSTPONE: R!
        POSTPONE: R'
        POSTPONE: R`
        POSTPONE: R(
        POSTPONE: R[
    }
}
{
    "resource:basis/roman/roman.factor"
    "roman"
    V{ POSTPONE: ROMAN-OP: POSTPONE: ROMAN: }
}
{
    "resource:basis/shuffle/shuffle.factor"
    "shuffle"
    V{ POSTPONE: shuffle( }
}
{
    "resource:basis/specialized-arrays/specialized-arrays.modern"
    "specialized-arrays"
    V{
        POSTPONE: SPECIALIZED-ARRAYS:
        POSTPONE: SPECIALIZED-ARRAY:
    }
}
{
    "resource:basis/specialized-vectors/specialized-vectors.modern"
    "specialized-vectors"
    V{
        POSTPONE: SPECIALIZED-VECTOR:
        POSTPONE: SPECIALIZED-VECTORS:
    }
}
{
    "resource:basis/suffix-arrays/suffix-arrays.factor"
    "suffix-arrays"
    V{ POSTPONE: SA{ }
}
{
    "resource:basis/tools/test/test.factor"
    "tools.test.private"
    V{ POSTPONE: TEST: }
}
{ "resource:basis/tr/tr.factor" "tr" V{ POSTPONE: TR: } }
{
    "resource:basis/tuple-arrays/tuple-arrays.modern"
    "tuple-arrays"
    V{ POSTPONE: TUPLE-ARRAY: }
}
{
    "resource:basis/typed/typed.factor"
    "typed"
    V{ POSTPONE: TYPED: POSTPONE: TYPED:: }
}
{
    "resource:basis/ui/pixel-formats/pixel-formats.modern"
    "ui.pixel-formats.private"
    V{ POSTPONE: PIXEL-FORMAT-ATTRIBUTE-TABLE: }
}
{
    "resource:basis/ui/ui.factor"
    "ui"
    V{ POSTPONE: MAIN-WINDOW: }
}
{
    "resource:basis/unicode/categories/syntax/syntax.factor"
    "unicode.categories.syntax"
    V{ POSTPONE: CATEGORY: POSTPONE: CATEGORY-NOT: }
}
{ "resource:basis/urls/urls.factor" "urls" V{ POSTPONE: URL" } }
{
    "resource:basis/vlists/vlists.factor"
    "vlists"
    V{ POSTPONE: VA{ POSTPONE: VL{ }
}
{
    "resource:basis/x11/syntax/syntax.factor"
    "x11.syntax"
    V{ POSTPONE: X-FUNCTION: }
}
{
    "resource:basis/xml/errors/errors.factor"
    "xml.errors"
    V{ POSTPONE: XML-ERROR: }
}
{
    "resource:basis/xml/syntax/syntax.factor"
    "xml.syntax"
    V{
        POSTPONE: TAGS:
        POSTPONE: <XML
        POSTPONE: TAG:
        POSTPONE: XML-NS:
        POSTPONE: [XML
    }
}
{
    "resource:basis/xmode/loader/syntax/syntax.factor"
    "xmode.loader.syntax"
    V{ POSTPONE: RULE: }
}
{
    "resource:core/syntax/syntax.factor"
    "syntax"
    V{
        POSTPONE: GENERIC:
        POSTPONE: DEFER:
        POSTPONE: IN:
        POSTPONE: V{
        POSTPONE: =======
        POSTPONE: BUILTIN:
        POSTPONE: recursive
        POSTPONE: SYNTAX:
        POSTPONE: flushable
        POSTPONE: GENERIC#
        POSTPONE: CS{
        POSTPONE: SLOT:
        POSTPONE: !
        POSTPONE: "
        POSTPONE: INTERSECTION:
        POSTPONE: W{
        POSTPONE: (
        POSTPONE: CONSTANT:
        POSTPONE: USE:
        POSTPONE: MATH:
        POSTPONE: SINGLETON:
        POSTPONE: :
        POSTPONE: <<<<<<
        POSTPONE: ALIAS:
        POSTPONE: M\
        POSTPONE: T{
        POSTPONE: B
        POSTPONE: SYMBOLS:
        POSTPONE: ERROR:
        POSTPONE: HS{
        POSTPONE: PRIVATE>
        POSTPONE: QUALIFIED-WITH:
        POSTPONE: SINGLETONS:
        POSTPONE: ======
        POSTPONE: [
        POSTPONE: \
        POSTPONE: UNION:
        POSTPONE: P"
        POSTPONE: NAN:
        POSTPONE: not{
        POSTPONE: f
        POSTPONE: final
        POSTPONE: inline
        POSTPONE: intersection{
        POSTPONE: POSTPONE:
        POSTPONE: delimiter
        POSTPONE: INSTANCE:
        POSTPONE: FROM:
        POSTPONE: {
        POSTPONE: maybe{
        POSTPONE: deprecated
        POSTPONE: HOOK:
        POSTPONE: B{
        POSTPONE: <PRIVATE
        POSTPONE: >>>>>>
        POSTPONE: CHAR:
        POSTPONE: <<<<<<<
        POSTPONE: M:
        POSTPONE: FORGET:
        POSTPONE: execute(
        POSTPONE: call(
        POSTPONE: #!
        POSTPONE: C{
        POSTPONE: PRIMITIVE:
        POSTPONE: >>>>>>>
        POSTPONE: MIXIN:
        POSTPONE: PREDICATE:
        POSTPONE: H{
        POSTPONE: EXCLUDE:
        POSTPONE: B:
        POSTPONE: BV{
        POSTPONE: TUPLE:
        POSTPONE: SBUF"
        POSTPONE: UNUSE:
        POSTPONE: union{
        POSTPONE: RENAME:
        POSTPONE: QUALIFIED:
        POSTPONE: C:
        POSTPONE: MAIN:
        POSTPONE: call-next-method
        POSTPONE: <<
        POSTPONE: USING:
        POSTPONE: SYMBOL:
        POSTPONE: foldable
    }
}
{
    "resource:extra/alien/data/map/map.factor"
    "alien.data.map"
    V{ POSTPONE: data-map( POSTPONE: data-map!( }
}
{
    "resource:extra/alien/fortran/fortran.factor"
    "alien.fortran"
    V{
        POSTPONE: SUBROUTINE:
        POSTPONE: FUNCTION:
        POSTPONE: LIBRARY:
    }
}
{
    "resource:extra/arrays/shaped/shaped.factor"
    "arrays.shaped"
    V{ POSTPONE: sa{ }
}
{
    "resource:extra/backticks/backticks.factor"
    "backticks"
    V{ POSTPONE: ` }
}
{
    "resource:extra/benchmark/raytracer-simd/raytracer-simd.factor"
    "benchmark.raytracer-simd"
    V{ POSTPONE: no-compile }
}
{
    "resource:extra/calendar/holidays/holidays.factor"
    "calendar.holidays"
    V{ POSTPONE: HOLIDAY: POSTPONE: HOLIDAY-NAME: }
}
{
    "resource:extra/classes/struct/vectored/vectored.factor"
    "classes.struct.vectored"
    V{ POSTPONE: VECTORED-STRUCT: }
}
{
    "resource:extra/code-arrays/code-arrays.factor"
    "code-arrays"
    V{ POSTPONE: {{ POSTPONE: [[ POSTPONE: V{{ POSTPONE: H{{ }
}
{
    "resource:extra/colors/flex-hex/flex-hex.factor"
    "colors.flex-hex"
    V{ POSTPONE: FLEXHEXCOLOR: }
}
{
    "resource:extra/constructors/constructors.factor"
    "constructors"
    V{ POSTPONE: CONSTRUCTOR: POSTPONE: SLOT-CONSTRUCTOR: }
}
{
    "resource:extra/cpu/8080/emulator/emulator.factor"
    "cpu.8080.emulator"
    V{
        POSTPONE: opcode
        POSTPONE: INSTRUCTION:
        POSTPONE: cycles
    }
}
{
    "resource:extra/cpu/arm/assembler/assembler.factor"
    "cpu.arm.assembler"
    V{ POSTPONE: REGISTER: }
}
{
    "resource:extra/cuda/syntax/syntax.factor"
    "cuda.syntax"
    V{
        POSTPONE: CUDA-GLOBAL:
        POSTPONE: CUDA-FUNCTION:
        POSTPONE: CUDA-LIBRARY:
    }
}
{
    "resource:extra/decimals/decimals.factor"
    "decimals"
    V{ POSTPONE: D: }
}
{
    "resource:extra/descriptive/descriptive.factor"
    "descriptive"
    V{ POSTPONE: DESCRIPTIVE:: POSTPONE: DESCRIPTIVE: }
}
{
    "resource:extra/dice/dice.factor"
    "dice"
    V{ POSTPONE: ROLL: }
}
{
    "resource:extra/fonts/syntax/syntax.factor"
    "fonts.syntax"
    V{ POSTPONE: FONT: }
}
{
    "resource:extra/game/worlds/worlds.factor"
    "game.worlds"
    V{ POSTPONE: GAME: }
}
{
    "resource:extra/gml/macros/macros.factor"
    "gml.macros"
    V{ POSTPONE: LOG-GML: }
}
{
    "resource:extra/gml/runtime/runtime.factor"
    "gml.runtime"
    V{
        POSTPONE: GML::
        POSTPONE: exec"
        POSTPONE: EXEC:
        POSTPONE: EXEC::
        POSTPONE: GML:
    }
}
{
    "resource:extra/gpu/render/render.factor"
    "gpu.render"
    V{ POSTPONE: UNIFORM-TUPLE: }
}
{
    "resource:extra/gpu/shaders/shaders.factor"
    "gpu.shaders"
    V{
        POSTPONE: feedback-format:
        POSTPONE: GLSL-SHADER:
        POSTPONE: VERTEX-STRUCT:
        POSTPONE: VERTEX-FORMAT:
        POSTPONE: GLSL-PROGRAM:
        POSTPONE: geometry-shader-vertices-out:
        POSTPONE: GLSL-SHADER-FILE:
    }
}
{
    "resource:extra/hash-sets/numbers/numbers.factor"
    "hash-sets.numbers"
    V{ POSTPONE: NHS{ }
}
{
    "resource:extra/hashtables/numbers/numbers.factor"
    "hashtables.numbers"
    V{ POSTPONE: NH{ }
}
{
    "resource:extra/infix/infix.factor"
    "infix"
    V{ POSTPONE: [infix POSTPONE: INFIX:: }
}
{
    "resource:extra/irc/messages/base/base.factor"
    "irc.messages.base"
    V{ POSTPONE: IRC: }
}
{
    "resource:extra/literate/literate.factor"
    "literate"
    V{ POSTPONE: <LITERATE }
}
{
    "resource:extra/llvm/types/types.factor"
    "llvm.types"
    V{ POSTPONE: TYPE: }
}
{
    "resource:extra/math/derivatives/syntax/syntax.factor"
    "math.derivatives.syntax"
    V{ POSTPONE: DERIVATIVE: }
}
{
    "resource:extra/math/extras/extras.factor"
    "math.extras"
    V{ POSTPONE: ... POSTPONE: .. }
}
{
    "resource:extra/memory/pools/pools.factor"
    "memory.pools"
    V{ POSTPONE: POOL: }
}
{
    "resource:extra/method-chains/method-chains.factor"
    "method-chains"
    V{ POSTPONE: AFTER: POSTPONE: BEFORE: }
}
{
    "resource:extra/money/money.factor"
    "money"
    V{ POSTPONE: DECIMAL: }
}
{
    "resource:extra/mongodb/driver/driver.factor"
    "mongodb.driver"
    V{ POSTPONE: r/ }
}
{
    "resource:extra/mongodb/tuple/tuple.factor"
    "mongodb.tuple"
    V{ POSTPONE: MDBTUPLE: }
}
{
    "resource:extra/morse/morse.factor"
    "morse"
    V{ POSTPONE: [MORSE }
}
{
    "resource:extra/multi-methods/multi-methods.factor"
    "multi-methods"
    V{ POSTPONE: GENERIC: POSTPONE: METHOD: POSTPONE: M: }
}
{
    "resource:extra/nested-comments/nested-comments.factor"
    "nested-comments"
    V{ POSTPONE: uhh(* }
}
{
    "resource:extra/opencl/syntax/syntax.factor"
    "opencl.syntax"
    V{ POSTPONE: SINGLETONS-UNION: }
}
{
    "resource:extra/pair-methods/pair-methods.factor"
    "pair-methods"
    V{ POSTPONE: PAIR-M: POSTPONE: PAIR-GENERIC: }
}
{
    "resource:extra/pair-rocket/pair-rocket.factor"
    "pair-rocket"
    V{ POSTPONE: => }
}
{
    "resource:extra/persistency/persistency.factor"
    "persistency"
    V{ POSTPONE: %" POSTPONE: STORED-TUPLE: }
}
{
    "resource:extra/poker/poker.factor"
    "poker"
    V{ POSTPONE: HAND{ }
}
{
    "resource:extra/project-euler/common/common.factor"
    "project-euler.common"
    V{ POSTPONE: SOLUTION: }
}
{
    "resource:extra/python/syntax/syntax.factor"
    "python.syntax"
    V{
        POSTPONE: PY-FROM:
        POSTPONE: PY-QUALIFIED-FROM:
        POSTPONE: PY-METHODS:
    }
}
{ "resource:extra/qw/qw.factor" "qw" V{ POSTPONE: qw{ } }
{
    "resource:extra/roles/roles.factor"
    "roles"
    V{ POSTPONE: TUPLE: POSTPONE: ROLE: }
}
{
    "resource:extra/slides/slides.factor"
    "slides"
    V{ POSTPONE: STRIP-TEASE: }
}
{
    "resource:extra/slots/syntax/syntax.factor"
    "slots.syntax"
    V{
        POSTPONE: slots[
        POSTPONE: slots{
        POSTPONE: set{
        POSTPONE: copy-slots{
        POSTPONE: set-slots[
        POSTPONE: set-slots{
        POSTPONE: get[
        POSTPONE: get{
        POSTPONE: set[
    }
}
{
    "resource:extra/smalltalk/selectors/selectors.factor"
    "smalltalk.selectors"
    V{ POSTPONE: SELECTOR: }
}
{
    "resource:extra/specialized/specialized.factor"
    "specialized"
    V{ POSTPONE: specialized }
}
{
    "resource:extra/trees/avl/avl.factor"
    "trees.avl"
    V{ POSTPONE: AVL{ }
}
{
    "resource:extra/trees/splay/splay.factor"
    "trees.splay"
    V{ POSTPONE: SPLAY{ }
}
{
    "resource:extra/trees/trees.factor"
    "trees"
    V{ POSTPONE: TREE{ }
}
{
    "resource:extra/units/reduction/reduction.factor"
    "units.reduction"
    V{ POSTPONE: STORAGE: }
}
{
    "resource:extra/variables/variables.factor"
    "variables"
    V{
        POSTPONE: GLOBAL:
        POSTPONE: set:
        POSTPONE: VAR:
        POSTPONE: TYPED-GLOBAL:
        POSTPONE: TYPED-VAR:
    }
}
{
    "resource:extra/variants/variants.factor"
    "variants"
    V{ POSTPONE: VARIANT-MEMBER: POSTPONE: VARIANT: }
}
{
    "resource:extra/vocabs/git/git.factor"
    "vocabs.git"
    V{ POSTPONE: USE-REV: }
}
{
    "resource:extra/xkcd/xkcd.factor"
    "xkcd"
    V{ POSTPONE: XKCD: }
}


SYMBOL: was-private?
 all-words [ "syntax" word-prop ] filter
[ vocabulary>> ] collect-by >alist
[ first2 [ [ ".private" ?tail drop modern-syntax-path ] keep ] dip 3array ] map
[
    f was-private? [
        dup first dup . utf8 [
            "! Copyright (C) 2015 Doug Coleman." print
            "! See http://factorcode.org/license.txt for BSD license." print
            "USING: ;" print
            [
                second ".private" ?tail [ ".syntax" append "IN: " prepend print nl ] dip
                [ "<PRIVATE" print was-private? on ] when
            ] [
                third natural-sort [ name>> "PARSER: " " ;" surround print ] each
            ] bi
            was-private? get [ "PRIVATE>" print ] when
        ] with-file-writer
    ] with-variable
] each


find . | grep '\-syntax.modern' | xargs cat
*/

(*

all-words [ "syntax" word-prop ] filter
[ vocabulary>> ] collect-by .


! words we define that aren't parsing words
parsers get-global keys
all-words [ parsing-word? ] filter
natural-sort [ name>> ] map diff
{
    "IMPORTS:"
    "NAME:"
    "PARSER:"
    "LITERAL-PARSER:"
    "AUTHOR:"
    "PACKAGE:"
}

! words we don't define
! not generated words
parsers get-global keys
all-words [ "syntax" word-prop ] filter
[ name>> ] map swap diff
natural-sort
[ . ] each

"\""
"$"
"${"
"&:"
"("
"->"
"/*"
"8-BIT:"
"<<<<<<"
"<<<<<<<"
"<EBNF"
"======"
"======="
">>>>>>"
">>>>>>>"
"?{"
"ALIEN:"
"B"
"B:"
"BACKWARD-ANALYSIS:"
"BAD-ALIEN"
"BROADCAST:"
"BV{"
"B{"
"CALLBACK:"
"CATEGORY-NOT:"
"CATEGORY:"
"CFSTRING:"
"CLASS:"
"COLOR:"
"CONSTRUCTOR:"
"CONSULT:"
"CS{"
"C{"
"D"
"DEFERS"
"DEFINES"
"DEFINES-CLASS"
"DEFINES-PRIVATE"
"DELIMITED:"
"DESTRUCTOR:"
"DLL\""
"DL{"
"FORWARD-ANALYSIS:"
"FRAMEWORK:"
"HEREDOC:"
"HS{"
"ICON:"
"IDENTITY-MEMO:"
"IDENTITY-MEMO::"
"IHS{"
"IH{"
"INTERSECTION:"
"IS"
"I["
"LOG:"
"MAIN-WINDOW:"
"METHOD:"
"M\\"
"NAN:"
"P\""
"PEG:"
"PIXEL-FORMAT-ATTRIBUTE-TABLE:"
"PRIVATE>"
"R"
"R!"
"R\""
"R#"
"R'"
"R("
"R/"
"R@"
"RECT:"
"RENAMING:"
"RESET"
"R["
"R`"
"R{"
"R|"
"S@"
"SBUF\""
"SEL:"
"SLOT-CONSTRUCTOR:"
"SLOT-PROTOCOL:"
"SPECIAL-OBJECT:"
"SUPER->"
"S{"
"TEST:"
"TIP:"
"TOKENIZER:"
"TR:"
"UNION-STRUCT:"
"UNUSE:"
"URL\""
"W{"
"X509_V_:"
"[EBNF"
"[let"
"c-array@"
"c-array{"
"call-next-method"
"delimiter"
"deprecated"
"eval("
"f"
"flags{"
"intersection{"
"maybe{"
"not{"
"shuffle("
"union{"


clear
SYMBOL: was-private?
 all-words [ "syntax" word-prop ] filter
[ vocabulary>> ] collect-by >alist

[ first2 [ [ ".private" ?tail drop modern-syntax-path ] keep ] dip 3array ] map
[ second "syntax" = ] filter
[
    f was-private? [
        dup first dup . utf8 [
            "! Copyright (C) 2015 Doug Coleman." print
            "! See http://factorcode.org/license.txt for BSD license." print
            "USING: ;" print
            [
                second ".private" ?tail [ ".syntax" append "IN: " prepend print nl ] dip
                [ "<PRIVATE" print was-private? on ] when
            ] [
                third natural-sort [
                    [ name>>

                        {
                            [ drop "PARSER: " ]
                            [ >lower dup length 1 > [ ":" ?tail drop ] when " " ]
                            [ parsers get ?at [ def>>  last def>> penultimate dup wrapper? [ wrapped>> "slots" word-prop [ name>> ] map " " join "{ " " } " surround ] [ drop "{ } " ] if ] [ drop "{ } " ] if  ]
                            [ " " ]
                            [
                              parsers get ?at
                              [ dup def>> last  def>> last \ slots>boa =
                                    [
                                      def>> but-last >array
                                      [ dup word? [ name>> ] when  ] map " " join
                                     ] [ def>> last name>> ] if
                              ]
                              [ drop "" ] if
                            ]
                            [ drop " ;" ]
                        } cleave
                    ] "" append-outputs-as print
                ] each
            ] bi
            was-private? get [ "PRIVATE>" print ] when
        ] with-file-writer
    ] with-variable
] each


*)

