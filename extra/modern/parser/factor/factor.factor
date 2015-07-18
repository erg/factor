! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs combinators combinators.smart
constructors io kernel make math modern.parser multiline
namespaces nested-comments sequences ;
IN: modern.parser.factor

PARSER: psyntax SYNTAX: raw body ;
PARSER: pparser PARSER: new-class raw body ;
PARSER: pcomment ! readln ;
PARSER: pshell-comment #! readln ;
PARSER: heredoc HEREDOC: token dup name>> multiline-string-until ;

PARSER: pin IN: token ;
PARSER: puse USE: token ;
PARSER: pusing USING: ";" raw-until ;
PARSER: pfrom FROM: token "=>" expect ";" parse-until ;
PARSER: pexclude EXCLUDE: token "=>" expect ";" parse-until ;
PARSER: prename RENAME: token "=>" expect token ;
PARSER: pqualified QUALIFIED: token ;
PARSER: pqualified-with QUALIFIED-WITH: token token ;

: parse-open-signature ( -- in sep out sep )
    "--" raw-until ")" raw-until ;
PARSER: psignature ( "--" raw-until ")" raw-until ;

PARSER: pexecute( execute( parse-open-signature ;
PARSER: pcall( call( parse-open-signature ;
PARSER: pdata-map( data-map( parse-open-signature ;
PARSER: pdata-map!( data-map!( parse-open-signature ;

PARSER: pconstant CONSTANT: token parse ;
PARSER: psymbol SYMBOL: token ;
PARSER: psymbols SYMBOLS: ";" raw-until ;
PARSER: ppostpone POSTPONE: token ;
PARSER: pdefer DEFER: token ;

PARSER: pescaped \ raw ;
PARSER: pmethod-literal M\ token token ;

PARSER: pchar CHAR: raw ;

PARSER: pfunction : new-identifier body ;
PARSER: pfunction-locals :: new-identifier body ;
PARSER: ptyped TYPED: new-identifier body ;
PARSER: ptyped-locals TYPED:: new-identifier body ;
PARSER: pmemo MEMO: new-identifier body ;
PARSER: pmemo-locals MEMO:: new-identifier body ;

PARSER: ptuple TUPLE: new-identifier body ;
PARSER: pstruct STRUCT: new-identifier body ;
PARSER: perror ERROR: new-identifier body ;
PARSER: pslot SLOT: token ;
PARSER: pconstructor C: token token ;

: c-arguments ( -- sep arguments sep ) "(" expect ")" raw-until ;
PARSER: pc-function FUNCTION: token new-identifier c-arguments ";" expect ;
PARSER: pfunction-alias FUNCTION-ALIAS: token token new-identifier c-arguments ";" expect ;
PARSER: px-function X-FUNCTION: token new-identifier c-arguments ";" expect ;
PARSER: pgl-function GL-FUNCTION: token new-identifier c-arguments ";" expect ;

PARSER: phints HINTS: parse body ;

PARSER: pbuiltin BUILTIN: token body ;
PARSER: pprimitive PRIMITIVE: new-word parse-psignature ;
PARSER: pmain MAIN: existing-word ;

PARSER: pgeneric GENERIC: new-class parse-psignature ;
PARSER: pgeneric# GENERIC# new-class token parse-psignature ;
PARSER: phook HOOK: new-class existing-word parse-psignature ;
PARSER: pmethod M: existing-class token body ;
PARSER: pmethod-locals M:: existing-class token body ;
PARSER: ppredicate PREDICATE: new-identifier "<" expect existing-class body ;
PARSER: pmixin MIXIN: new-class ;
PARSER: pinstance INSTANCE: existing-class existing-class ;
PARSER: psingleton SINGLETON: new-class ;
PARSER: psingletons SINGLETONS: body ;
PARSER: pimport IMPORT: token ;
PARSER: pimports IMPORTS: ";" raw-until ;

PARSER: pmath MATH: new-word parse-psignature ;
PARSER: punion UNION: new-class body ;
PARSER: pcolor COLOR: token ;

PARSER: pspecialized-array SPECIALIZED-ARRAY: token ;
PARSER: pspecialized-arrays SPECIALIZED-ARRAYS: ";" raw-until ;

PARSER: pglsl-shader GLSL-SHADER: token token "\n;" multiline-string-until ;
PARSER: pglsl-program GLSL-PROGRAM: token ";" parse-until ;
PARSER: puniform-tuple UNIFORM-TUPLE: token ";" parse-until ;

PARSER: pebnf EBNF: token ";ENBF" multiline-string-until ;
PARSER: pebnf-acute <EBNF token "EBNF>" multiline-string-until ;
PARSER: pebnf-bracket [EBNF token "EBNF>" multiline-string-until ;

PARSER: pslots-quot slots[ "]" parse-until ;
PARSER: pslots-array slots{ "}" parse-until ;
PARSER: pset-slots-quot set-slots[ "]" parse-until ;
PARSER: pset-slots-array set-slots{ "}" parse-until ;
PARSER: pcopy-slots-array copy-slots{ "}" parse-until ;
PARSER: pset-quot set[ "]" parse-until ;
PARSER: pset-array set{ "}" parse-until ;
PARSER: pget-quot get[ "]" parse-until ;
PARSER: pget-array get{ "}" parse-until ;

PARSER: pblock [ "]" parse-until ;
PARSER: pfry '[ "]" parse-until ;
PARSER: pblock-eval $[ "]" parse-until ;
PARSER: pblock-locals [| "]" parse-until ;
PARSER: parray { "}" parse-until ;
PARSER: pvector V{ "}" parse-until ;
PARSER: phashtable H{ "}" parse-until ;

HEREDOC: omg
! PARSER: pparser LITERAL-PARSER: new-class raw body ;
! Doesn't look for (* inside strings, only finds it as raw
TUPLE: mnested-comment < parsed ;
CONSTRUCTOR: <mnested-comment> mnested-comment ( -- nested-comment ) ;
: parse-nested-comment' ( level -- )
    raw {
        { [ dup "(*" = ] [ drop 1 + parse-nested-comment' ] }
        { [ dup "*)" = ] [ drop 1 - dup zero? [ drop ] [ parse-nested-comment' ] if ] }
        { [ dup f = ] [ "*)" expected ] }
        [ drop parse-nested-comment' ]
    } cond ;

: parse-nested-comment ( -- nested-comment )
    1 parse-nested-comment' <mnested-comment> ;
\ parse-nested-comment "(*" register-parser



TUPLE: single-bind < parsed target ;
TUPLE: multi-bind < parsed targets ;
CONSTRUCTOR: <single-bind> single-bind ( target -- bind ) ;
CONSTRUCTOR: <multi-bind> multi-bind ( targets -- bind ) ;
: parse-bind ( -- bind )
    raw dup "(" = [
        drop ")" raw-until <multi-bind>
    ] [
        <single-bind>
    ] if ;
\ parse-bind ":>" register-parser


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
                  drop parse-array
                  "}" parse-until swap prefix <tuple-literal-assoc>
              ]
        }
        { "}" [ drop f <tuple-literal-boa> ] }
        [ malformed-tuple-literal ]
    } case ;
\ parse-tuple-literal "T{" register-parser



TUPLE: functor < parsed name signature definitions ;
CONSTRUCTOR: <functor> functor ( name signature definitions -- functor ) ;
: parse-functor ( -- functor )
    token parse-signature(--) ";FUNCTOR" parse-until <functor> ;
\ parse-functor "FUNCTOR:" register-parser

TUPLE: functor-syntax < parsed name body ;
CONSTRUCTOR: <functor-syntax> functor-syntax ( name body -- functor ) ;
: parse-functor-syntax ( -- functor )
    token body <functor-syntax> ;
\ parse-functor-syntax "FUNCTOR-SYNTAX:" register-parser

TUPLE: name < parsed name target ;
CONSTRUCTOR: <name> name ( name target -- object ) ;
: parse-name ( -- name )
    token token <name> ;
\ parse-name "NAME:" register-parser

TUPLE: compilation-unit < parsed code ;
CONSTRUCTOR: <compilation-unit> compilation-unit ( code -- compilation-unit ) ;
: parse-compilation-unit ( -- compilation-unit )
    ">>" parse-until <compilation-unit> ;
\ parse-compilation-unit "<<" register-parser


TUPLE: typedef < parsed old new ;
CONSTRUCTOR: <typedef> typedef ( old new -- typedef ) ;
: parse-typedef ( -- typedef )
    token token <typedef> ;
\ parse-typedef "TYPEDEF:" register-parser

TUPLE: library < parsed name ;
CONSTRUCTOR: <library> library ( name -- library ) ;
: parse-library ( -- library )
    token <library> ;
\ parse-library "LIBRARY:" register-parser

TUPLE: c-type < parsed name ;
CONSTRUCTOR: <c-type> c-type ( name -- c-type ) ;
: parse-c-type ( -- c-type )
    token <c-type> ;
\ parse-c-type "C-TYPE:" register-parser

TUPLE: mmacro < parsed name signature body ;
CONSTRUCTOR: <mmacro> mmacro ( name signature body -- macro ) ;
: parse-macro ( -- macro )
    token parse-signature(--) ";" parse-until <mmacro> ;
\ parse-macro "MACRO:" register-parser

TUPLE: locals-macro < parsed name signature body ;
CONSTRUCTOR: <locals-macro> locals-macro ( name signature body -- macro ) ;
: parse-locals-macro ( -- macro )
    token parse-signature(--) ";" parse-until <locals-macro> ;
\ parse-locals-macro "MACRO::" register-parser

TUPLE: packed-struct < parsed name slots ;
CONSTRUCTOR: <packed-struct> packed-struct ( name slots -- packed-struct ) ;
: parse-packed-struct ( -- struct )
    token ";" parse-until <packed-struct> ;
\ parse-packed-struct "PACKED-STRUCT:" register-parser

TUPLE: union-struct < parsed name slots ;
CONSTRUCTOR: <union-struct> union-struct ( name slots -- union-struct ) ;
: parse-union-struct ( -- union-struct )
    token ";" parse-until <union-struct> ;
\ parse-union-struct "UNION-STRUCT:" register-parser

TUPLE: alias < parsed name target ;
CONSTRUCTOR: <alias> alias ( name target -- alias ) ;
: parse-alias ( -- alias )
    token token <alias> ;
\ parse-alias "ALIAS:" register-parser

TUPLE: mregisters < parsed names ;
CONSTRUCTOR: <mregisters> mregisters ( names -- obj ) ;
: parse-registers ( -- obj )
    ";" parse-until <mregisters> ;
\ parse-registers "REGISTERS:" register-parser

TUPLE: mhi-registers < parsed names ;
CONSTRUCTOR: <mhi-registers> mhi-registers ( names -- obj ) ;
: parse-hi-registers ( -- obj )
    ";" parse-until <mhi-registers> ;
\ parse-hi-registers "HI-REGISTERS:" register-parser

TUPLE: about < parsed name ;
CONSTRUCTOR: <about> about ( name -- obj ) ;
: parse-about ( -- obj )
    token <about> ;
\ parse-about "ABOUT:" register-parser

TUPLE: article < parsed name body ;
CONSTRUCTOR: <article> article ( name body -- obj ) ;
: parse-article ( -- obj )
    token ";" parse-until <article> ;
\ parse-article "ARTICLE:" register-parser

TUPLE: c-global < parsed type name ;
CONSTRUCTOR: <c-global> c-global ( type name -- obj ) ;
: parse-c-global ( -- obj )
    token token <c-global> ;
\ parse-c-global "C-GLOBAL:" register-parser

TUPLE: protocol < parsed name functions ;
CONSTRUCTOR: <protocol> protocol ( name functions -- obj ) ;
: parse-protocol ( -- obj )
    token ";" parse-until <protocol> ;
\ parse-protocol "PROTOCOL:" register-parser

TUPLE: mfoldable-insn < parsed name body ;
CONSTRUCTOR: <mfoldable-insn> mfoldable-insn ( name body -- obj ) ;
: parse-foldable-insn ( -- obj )
    token ";" parse-until <mfoldable-insn> ;
\ parse-foldable-insn "FOLDABLE-INSN:" register-parser

TUPLE: mflushable-insn < parsed name body ;
CONSTRUCTOR: <mflushable-insn> mflushable-insn ( name body -- obj ) ;
: parse-flushable-insn ( -- obj )
    token ";" parse-until <mflushable-insn> ;
\ parse-flushable-insn "FLUSHABLE-INSN:" register-parser

TUPLE: mvreg-insn < parsed name body ;
CONSTRUCTOR: <mvreg-insn> mvreg-insn ( name body -- obj ) ;
: parse-vreg-insn ( -- obj )
    token ";" parse-until <mvreg-insn> ;
\ parse-vreg-insn "VREG-INSN:" register-parser

TUPLE: minsn < parsed name body ;
CONSTRUCTOR: <minsn> minsn ( name body -- obj ) ;
: parse-insn ( -- obj )
    token ";" parse-until <minsn> ;
\ parse-insn "INSN:" register-parser

TUPLE: codegen < parsed name1 name2 ;
CONSTRUCTOR: <codegen> codegen ( name1 name2 -- obj ) ;
: parse-codegen ( -- obj )
    token token <codegen> ;
\ parse-codegen "CODEGEN:" register-parser

TUPLE: conditional < parsed name1 name2 ;
CONSTRUCTOR: <conditional> conditional ( name1 name2 -- obj ) ;
: parse-conditional ( -- obj )
    token token <conditional> ;
\ parse-conditional "CONDITIONAL:" register-parser

TUPLE: simd-128 < parsed name ;
CONSTRUCTOR: <simd-128> simd-128 ( name -- obj ) ;
: parse-simd-128 ( -- obj )
    token <simd-128> ;
\ parse-simd-128 "SIMD-128:" register-parser

TUPLE: simd-128-cord < parsed name1 name2 ;
CONSTRUCTOR: <simd-128-cord> simd-128-cord ( name1 name2 -- obj ) ;
: parse-simd-128-cord ( -- obj )
    token token <simd-128-cord> ;
\ parse-simd-128-cord "SIMD-128-CORD:" register-parser

TUPLE: simd-intrinsic < parsed name body ;
CONSTRUCTOR: <simd-intrinsic> simd-intrinsic ( name body -- obj ) ;
: parse-simd-intrinsic ( -- obj )
    token ";" parse-until <simd-intrinsic> ;
\ parse-simd-intrinsic "SIMD-INTRINSIC:" register-parser

TUPLE: locals-simd-intrinsic < parsed name body ;
CONSTRUCTOR: <locals-simd-intrinsic> locals-simd-intrinsic ( name body -- obj ) ;
: parse-locals-simd-intrinsic ( -- obj )
    token ";" parse-until <locals-simd-intrinsic> ;
\ parse-locals-simd-intrinsic "SIMD-INTRINSIC::" register-parser

TUPLE: menum < parsed name slots ;
CONSTRUCTOR: <menum> menum ( name slots -- obj ) ;
: parse-enum ( -- obj )
    token ";" parse-until <menum> ;
\ parse-enum "ENUM:" register-parser

TUPLE: forget < parsed name ;
CONSTRUCTOR: <forget> forget ( name -- obj ) ;
: parse-forget ( -- obj )
    token <forget> ;
\ parse-forget "FORGET:" register-parser

TUPLE: mpointer < parsed to ;
CONSTRUCTOR: <mpointer> mpointer ( to -- obj ) ;
: parse-pointer ( -- obj )
    token <mpointer> ;
\ parse-pointer "pointer:" register-parser

TUPLE: help < parsed name body ;
CONSTRUCTOR: <help> help ( name body -- obj ) ;
: parse-help ( -- help )
    token
    ";" parse-until <help> ;
\ parse-help "HELP:" register-parser

TUPLE: long-string < parsed name text ;
CONSTRUCTOR: <long-string> long-string ( name text -- long-string ) ;
: parse-long-string ( -- long-string )
    token "\n;" multiline-string-until <long-string> ;
\ parse-long-string "STRING:" register-parser

TUPLE: mirc < parsed name command body ;
CONSTRUCTOR: <mirc> mirc ( name command body -- mirc ) ;
: parse-irc ( -- irc )
    token parse ";" raw-until <mirc> ;
\ parse-irc "IRC:" register-parser

! The above is enough to kind of correctly parse factor.

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


! TUPLE: munit-test < parsed ;
! CONSTRUCTOR: <munit-test> munit-test ( -- obj ) ;
! : parse-unit-test ( -- munit-test ) <munit-test> ;
! \ parse-unit-test "unit-test" register-parser





TUPLE: let-block < parsed body ;
CONSTRUCTOR: <let-block> let-block ( body -- block ) ;
: parse-let-block ( -- let-block )
    "]" parse-until <let-block> ;
\ parse-let-block "[let" register-parser


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
*)
omg
drop
