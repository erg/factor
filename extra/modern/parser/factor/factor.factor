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
PARSER: pforget FORGET: token ;

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
PARSER: palias ALIAS: token token ;
PARSER: ptyped TYPED: new-identifier body ;
PARSER: ptyped-locals TYPED:: new-identifier body ;
PARSER: pmemo MEMO: new-identifier body ;
PARSER: pmemo-locals MEMO:: new-identifier body ;
PARSER: pmacro MACRO: new-identifier body ;
PARSER: pmacro-locals MACRO:: new-identifier body ;

PARSER: ptuple TUPLE: new-identifier body ;
PARSER: pstruct STRUCT: new-identifier body ;
PARSER: ppacked-struct PACKED-STRUCT: new-identifier body ;
PARSER: punion-struct UNION-STRUCT: new-identifier body ;
PARSER: perror ERROR: new-identifier body ;
PARSER: pslot SLOT: token ;
PARSER: pconstructor C: token token ;

PARSER: pfunctor FUNCTOR: token parse-psignature ";FUNCTOR" parse-until ;
PARSER: pfunctor-syntax FUNCTOR-SYNTAX: token body ;

: c-arguments ( -- sep arguments sep ) "(" expect ")" raw-until ;
PARSER: pc-function FUNCTION: token new-identifier c-arguments ";" expect ;
PARSER: pfunction-alias FUNCTION-ALIAS: token token new-identifier c-arguments ";" expect ;
PARSER: px-function X-FUNCTION: token new-identifier c-arguments ";" expect ;
PARSER: pgl-function GL-FUNCTION: token new-identifier c-arguments ";" expect ;
PARSER: pc-callback CALLBACK: token token c-arguments ";" expect ;
PARSER: ptypedef TYPEDEF: token token ;
PARSER: plibrary LIBRARY: token ;
PARSER: pc-type C-TYPE: token ;
PARSER: pc-global C-GLOBAL token token ;

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

PARSER: pspecial-object SPECIAL-OBJECT: token parse ;
PARSER: pmath MATH: new-word parse-psignature ;
PARSER: punion UNION: new-class body ;
PARSER: pintersection INTERSECTION: token ";" parse-until ;

PARSER: punicode-category CATEGORY: token token ;
PARSER: punicode-category-not CATEGORY-NOT: token token ;

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
PARSER: plet-block [let "]" parse-until ;
PARSER: pfry '[ "]" parse-until ;
PARSER: pblock-eval $[ "]" parse-until ;
PARSER: pblock-locals [| "]" parse-until ;
PARSER: parray { "}" parse-until ;
PARSER: pvector V{ "}" parse-until ;
PARSER: phashtable H{ "}" parse-until ;
PARSER: ptuple-literal T{ "}" parse-until ;

PARSER: pregisters REGISTERS: ";" parse-until ;
PARSER: phi-registers HI-REGISTERS: ";" parse-until ;
PARSER: pcolor COLOR: token ;
PARSER: ptest TEST: token ;

PARSER: pabout ABOUT: token ;
PARSER: particle ARTICLE: token ";" parse-until ;
PARSER: protocol PROTOCOL: token ";" parse-until ;
PARSER: pinsn PINSN: token ";" parse-until ;
PARSER: pvreg-insn VREG-INSN: token ";" parse-until ;
PARSER: pcodegen CODEGEN: token token ;
PARSER: pconditional CONDITIONAL: token token ;
PARSER: psimd-128 SIMD-128: token ;
PARSER: psimd-128-cord SIMD-128-CORD: token token ;
PARSER: psimd-instrinsic SIMD-INTRINSIC: token ";" parse-until ;
PARSER: psimd-instrinsic-locals SIMD-INTRINSIC:: token ";" parse-until ;
PARSER: penum ENUM: token ";" parse-until ;
PARSER: ppointer pointer: token ;
PARSER: phelp HELP: token ";" parse-until ;
PARSER: pname NAME: token token ;
PARSER: ptr TR: token ";" parse-until ;

PARSER: pcompilation-unit << ">>" parse-until ;
PARSER: plong-string STRING: token "\n;" multiline-string-until ;
: parse-pnested-comment' ( level -- )
    raw dup object>> {
        { [ dup "(*" = ] [ drop , 1 + parse-pnested-comment' ] }
        { [ dup "*)" = ] [ drop ptext pbecome , 1 - dup zero? [ drop ] [ parse-pnested-comment' ] if ] }
        { [ dup f = ] [ "*)" expected ] } ! failed
        [ drop , parse-pnested-comment' ]
    } cond ;
PARSER: pnested-comment (* [ 1 parse-pnested-comment' ] { } make ;

PARSER: pmirc MIRC: token parse ";" raw-until ;
PARSER: pconstructor-new CONSTRUCTOR: token token parse-psignature ";" parse-until ;
PARSER: pmain-window MAIN-WINDOW: token parse body ;
PARSER: pgame GAME: token parse body ;
PARSER: psolution SOLUTION: token ;
PARSER: p8-bit 8-BIT: token token token ;



HEREDOC: omg
! PARSER: pparser LITERAL-PARSER: new-class raw body ;
! Doesn't look for (* inside strings, only finds it as raw

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




! The above is enough to kind of correctly parse factor.



! TUPLE: munit-test < parsed ;
! CONSTRUCTOR: <munit-test> munit-test ( -- obj ) ;
! : parse-unit-test ( -- munit-test ) <munit-test> ;
! \ parse-unit-test "unit-test" register-parser

! TUPLE: tuple-literal-assoc < psequence ;
! TUPLE: tuple-literal-boa < psequence ;
! ERROR: malformed-tuple-literal ;
! CONSTRUCTOR: <tuple-literal-assoc> tuple-literal-assoc ( name slots -- tuple-literal ) ;
! CONSTRUCTOR: <tuple-literal-boa> tuple-literal-boa ( name slots -- tuple-literal ) ;
: parse-tuple-literal ( -- token token token )
B
    token
    token dup dup [ object>> ] when {
        { "f" [ "}" parse-until prefix <tuple-literal-boa> ] }
        ! { "{" [
                  ! parse-parray
                  ! "}" parse-until swap prefix ! <tuple-literal-assoc>
              ! ]
        ! }
        ! { "}" [ f ] } ! <tuple-literal-boa> ] }
        [ malformed-tuple-literal ]
    } case ; inline





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
