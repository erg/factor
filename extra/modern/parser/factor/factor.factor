! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators combinators.smart
constructors io kernel make math modern.parser multiline
namespaces nested-comments sequences ;
IN: modern.parser.factor

PARSER: psyntax SYNTAX: raw body ;
PARSER: pparser PARSER: raw raw body ;
PARSER: pcomment ! readln ;
PARSER: pshell-comment #! readln ;
PARSER: heredoc HEREDOC: token dup object>> multiline-string-until ;
! PARSER: pdelimited DELIMITED: ;

PARSER: pin IN: token ;
PARSER: puse USE: token ;
PARSER: punuse UNUSE: token ;
PARSER: pusing USING: ";" raw-until ;
PARSER: pfrom FROM: token "=>" expect ";" raw-until ;
PARSER: pexclude EXCLUDE: token "=>" expect ";" raw-until ;
PARSER: prename RENAME: raw raw "=>" expect raw ;
PARSER: pqualified QUALIFIED: token ;
PARSER: pqualified-with QUALIFIED-WITH: token token ;
PARSER: pforget FORGET: token ;

: parse-pinputs ( -- inputs sep )
    "--" raw-until [ pinputs boa ] dip ;

: parse-poutputs ( -- outputs sep )
    ")" raw-until [ poutputs boa ] dip ;

: parse-open-signature ( -- in sep out sep )
    parse-pinputs parse-poutputs ;

PARSER: psignature ( parse-open-signature ;

! Hacky, should be able to call parse-psignature with its "(" expect
: parse-entire-signature ( -- seq )
    [ "(" expect parse-open-signature ] output>array psignature boa ;

PARSER: pexecute( execute( parse-open-signature ;
PARSER: pcall( call( parse-open-signature ;
PARSER: peval( eval( parse-open-signature ;
PARSER: pdata-map( data-map( parse-open-signature ;
PARSER: pdata-map!( data-map!( parse-open-signature ;

PARSER: pconstant CONSTANT: token parse ;
PARSER: psymbol SYMBOL: token ;
PARSER: psymbols SYMBOLS: ";" raw-until ;
PARSER: ppostpone POSTPONE: raw ;
PARSER: pdefer DEFER: token ;

PARSER: pescaped \ raw ;
PARSER: pmethod-literal M\ token token ;

PARSER: pchar CHAR: raw ;
PARSER: palien ALIEN: token ;
PARSER: BAD-ALIEN BAD-ALIEN ;

PARSER: pfunction : new-identifier parse-entire-signature body ;
PARSER: pfunction-locals :: new-identifier parse-entire-signature body ;
PARSER: palias ALIAS: new-word existing-word ;
PARSER: ptyped TYPED: new-identifier parse-entire-signature body ;
PARSER: ptyped-locals TYPED:: new-identifier parse-entire-signature body ;
PARSER: pmemo MEMO: new-identifier parse-entire-signature body ;
PARSER: pmemo-locals MEMO:: new-identifier parse-entire-signature body ;
PARSER: pmacro MACRO: new-identifier parse-entire-signature body ;
PARSER: pmacro-locals MACRO:: new-identifier parse-entire-signature body ;
PARSER: ppeg PEG: new-identifier parse-entire-signature body ;

PARSER: ptuple TUPLE: new-identifier body ;
PARSER: pstruct STRUCT: new-identifier body ;
PARSER: ppacked-struct PACKED-STRUCT: new-identifier body ;
PARSER: punion-struct UNION-STRUCT: new-identifier body ;
PARSER: perror ERROR: new-identifier body ;
PARSER: pslot SLOT: token ;
PARSER: pconstructor C: token token ;
PARSER: pconstructor-new CONSTRUCTOR: token token parse-entire-signature body ;

PARSER: pfunctor FUNCTOR: token parse-entire-signature ";FUNCTOR" parse-until ;
PARSER: pfunctor-syntax FUNCTOR-SYNTAX: token body ;

: c-arguments ( -- sep arguments sep ) "(" expect ")" raw-until ;
PARSER: pc-function FUNCTION: token new-identifier c-arguments ";" expect ;
PARSER: pfunction-alias FUNCTION-ALIAS: token token new-identifier c-arguments ";" expect ;
PARSER: px-function X-FUNCTION: token new-identifier c-arguments ";" expect ;
PARSER: pgl-function GL-FUNCTION: token new-identifier parse c-arguments ";" expect ;
PARSER: pc-callback CALLBACK: token token c-arguments ";" expect ;
PARSER: psubroutine SUBROUTINE: token c-arguments ";" expect ;
PARSER: ptypedef TYPEDEF: token token ;
PARSER: plibrary LIBRARY: token ;
PARSER: pc-type C-TYPE: token ;
PARSER: pc-global C-GLOBAL token token ;

PARSER: phints HINTS: parse body ;

PARSER: pbuiltin BUILTIN: token body ;
PARSER: pprimitive PRIMITIVE: new-word parse-entire-signature ;
PARSER: pmain MAIN: existing-word ;

PARSER: pgeneric GENERIC: new-class parse-entire-signature ;
PARSER: pgeneric# GENERIC# new-class token parse-entire-signature ;
PARSER: phook HOOK: new-class existing-word parse-entire-signature ;
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
PARSER: pmath MATH: new-word parse-entire-signature ;
PARSER: punion UNION: new-class body ;
PARSER: pintersection INTERSECTION: token body ;

PARSER: punicode-category CATEGORY: token token ;
PARSER: punicode-category-not CATEGORY-NOT: token token ;

PARSER: pspecialized-array SPECIALIZED-ARRAY: token ;
PARSER: pspecialized-arrays SPECIALIZED-ARRAYS: ";" raw-until ;

PARSER: pglsl-shader GLSL-SHADER: token token "\n;" multiline-string-until ;
PARSER: pglsl-program GLSL-PROGRAM: token body ;
PARSER: puniform-tuple UNIFORM-TUPLE: token body ;

PARSER: pebnf EBNF: token ";EBNF" multiline-string-until ;
PARSER: pebnf-acute <EBNF token "EBNF>" multiline-string-until ;
PARSER: pebnf-bracket [EBNF token "EBNF]" multiline-string-until ;

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
PARSER: pbitset ?{ "}" parse-until ;
PARSER: pbyte-array B{ "}" parse-until ;
PARSER: pbyte-vector BV{ "}" parse-until ;
PARSER: phashtable H{ "}" parse-until ;
PARSER: phash-set HS{ "}" parse-until ;
PARSER: ptuple-literal T{ existing-class "}" parse-until ;
PARSER: pcallstack-literal CS{ "}" parse-until ;
PARSER: pcomplex-literal C{ "}" parse-until ;
PARSER: pdlist-literal DL{ "}" parse-until ;

PARSER: pflags{ flags{ "}" parse-until ;
PARSER: pintersection{ intersection{ "}" parse-until ;
PARSER: pmaybe{ maybe{ "}" parse-until ;
PARSER: pnot{ not{ "}" parse-until ;
PARSER: punion{ union{ "}" parse-until ;
PARSER: pshuffle( shuffle( ")" parse-until ;

PARSER: pc-array{ c-array{ "}" parse-until ;
PARSER: pwrapper-literal W{ "}" parse-until ;
PARSER: pstruct-literal S{ "}" parse-until ;
PARSER: pstruct-literal-at S@ token parse ;
PARSER: pinterpolate I[ "]I" multiline-string-until ;
PARSER: pidentity-hash-set IHS{ "}" parse-until ;
PARSER: pidentity-hashtable IH{ "}" parse-until ;

PARSER: pregisters REGISTERS: body ;
PARSER: phi-registers HI-REGISTERS: body ;
PARSER: pcolor COLOR: token ;
PARSER: ptest TEST: token ;

PARSER: pabout ABOUT: token ;
PARSER: particle ARTICLE: token body ;
PARSER: protocol PROTOCOL: token body ;

PARSER: pinsn INSN: new-word body ;
PARSER: pvreg-insn VREG-INSN: token body ;
PARSER: pflushable-insn FLUSHABLE-INSN: new-word body ;
PARSER: pfoldable-insn FOLDABLE-INSN: new-word body ;

PARSER: pcodegen CODEGEN: token token ;
PARSER: pconditional CONDITIONAL: token token ;
PARSER: psimd-128 SIMD-128: token ;
PARSER: psimd-128-cord SIMD-128-CORD: token token ;
PARSER: psimd-instrinsic SIMD-INTRINSIC: token body ;
PARSER: psimd-instrinsic-locals SIMD-INTRINSIC:: token body ;
PARSER: penum ENUM: token body ;
PARSER: ppointer pointer: token ;
PARSER: phelp HELP: token body ;
PARSER: pname NAME: token token ;
PARSER: ptr TR: token body ;

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

PARSER: pmirc IRC: token parse ";" raw-until ;
PARSER: pmain-window MAIN-WINDOW: token parse body ;
PARSER: pgame GAME: token parse body ;
PARSER: psolution SOLUTION: token ;
PARSER: p8-bit 8-BIT: token token token ;

: parse-bind ( -- seq )
    raw dup dup [ object>> ] when "(" = [
        ")" raw-until 3array
    ] when ;
PARSER: pbind :> parse-bind ;


PARSER: pdelimiter delimiter ;
PARSER: pdeprecated deprecated ;
PARSER: pf f ;
PARSER: pfinal final ;
PARSER: pflushable flushable ;
PARSER: pfoldable foldable ;
PARSER: pinline inline ;
PARSER: precursive recursive ;
PARSER: pprivate-begin <PRIVATE ;
PARSER: pprivate-end PRIVATE> ;
PARSER: pd-register D ;
PARSER: pr-register R ;
PARSER: pbreakpoint B ;
PARSER: pbreakpoint-parse-time B: ;




PARSER: pbackward-analysis BACKWARD-ANALYSIS: token ;
PARSER: pforward-analysis FORWARD-ANALYSIS: token ;

PARSER: plog LOG: token token ;
PARSER: pnan NAN: token ;
PARSER: pbroadcast BROADCAST: existing-word existing-class parse ;
PARSER: pconsult CONSULT: new-word existing-class body ;

PARSER: pcfstring CFSTRING: new-word parse ;
! PARSER: pclass CLASS: new-class "<" expect existing-class parse ;
! PARSER: pcocoa-method METHOD: "[" parse-until "]" parse-until ;
PARSER: pframework FRAMEWORK: parse ;
! PARSER: pcocoa-selector -> token ;
! PARSER: psuper-selector SUPER-> token ;


PARSER: pdestructor DESTRUCTOR: existing-word ;
PARSER: picon ICON: new-word token ;
PARSER: pidentity-memo IDENTITY-MEMO: ;
PARSER: pidentity-memo-locals IDENTITY-MEMO:: new-word parse-entire-signature body ;
PARSER: pmatch-vars MATCH-VARS: body ;
PARSER: PIXEL-FORMAT-ATTRIBUTE-TABLE: PIXEL-FORMAT-ATTRIBUTE-TABLE: ;
PARSER: RECT: RECT: ;


PARSER: peval-dollar $ parse ;
PARSER: peval-dollar-array ${ "}" parse-until ;
PARSER: c-global-literal &: token ;

PARSER: RESET RESET ;
PARSER: SEL: SEL: ;
PARSER: SLOT-CONSTRUCTOR: SLOT-CONSTRUCTOR: ;
PARSER: SLOT-PROTOCOL: SLOT-PROTOCOL: ;
PARSER: TIP: TIP: ;
PARSER: TOKENIZER: TOKENIZER: ;
PARSER: X509_V_: X509_V_: ;
PARSER: c-array@ c-array@ ;
PARSER: call-next-method call-next-method ;


/*
! PARSER: /* /* ;
! PARSER: <<<<<< <<<<<< ;
! PARSER: <<<<<<< <<<<<<< ;
! PARSER: ====== ====== ; ! remove
! PARSER: ======= ======= ; ! remove
! PARSER: >>>>>> >>>>>> ; ! remove
! PARSER: >>>>>>> >>>>>>> ; ! remove

! functors
! PARSER: IS IS ;
! PARSER: DEFERS DEFERS ;
! PARSER: DEFINES DEFINES ;
! PARSER: DEFINES-CLASS DEFINES-CLASS ;
! PARSER: DEFINES-PRIVATE DEFINES-PRIVATE ;
! PARSER: palien-address ALIEN: token ;
! PARSER: prenaming RENAMING: new-word parse parse parse ;

! string literals
! PARSER: " " ;
! PARSER: P" P" ;
! PARSER: URL" URL" ;
! PARSER: SBUF" SBUF" ;
! PARSER: DLL" DLL" ;

! Bootstrap


PARSER: R! R! ;
PARSER: R" R" ;
PARSER: R# R# ;
PARSER: R' R' ;
PARSER: R( R( ;
PARSER: R/ R/ ;
PARSER: R@ R@ ;
PARSER: R[ R[ ;
PARSER: R` R` ;
PARSER: R{ R{ ;
PARSER: R| R| ;
*/
