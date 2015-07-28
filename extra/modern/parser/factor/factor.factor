! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators combinators.smart
constructors io kernel make math modern.parser multiline
namespaces sequences ;
IN: modern.parser.factor

! Test cases:
! ALIAS: foo{ bar{
! HELP: foo{

! Can go away:
PARSER: heredoc HEREDOC: token-to-find object>> multiline-string-until ;
! string literals
! PARSER: " " ;
! PARSER: P" P" ;
! PARSER: URL" URL" ;
! PARSER: SBUF" SBUF" ;
! PARSER: DLL" DLL" ;

PARSER: pcompilation-unit-begin << ; ! going away
PARSER: pcompilation-unit-end >> ; ! going away


PARSER: regexp-/ R/ "/" multiline-string-until ;
PARSER: regexp-# R# "#" multiline-string-until ;
PARSER: regexp-' R' "'" multiline-string-until ;
PARSER: regexp-( R( "(" multiline-string-until ;
PARSER: regexp-@ R@ "@" multiline-string-until ;
PARSER: regexp-` R` "`" multiline-string-until ;
PARSER: regexp-| R| "|" multiline-string-until ;
PARSER: regexp-! R! "!" multiline-string-until ;


! words[
! PARSER: pblock [ "]" parse-until ;
! PARSER: pfry '[ "]" parse-until ;
! PARSER: pblock-eval $[ "]" parse-until ;
! PARSER: pblock-locals [| "]" parse-until ;
! PARSER: pset-quot set[ "]" parse-until ;
! PARSER: pget-quot get[ "]" parse-until ;
! PARSER: pslots-quot slots[ "]" parse-until ;
! PARSER: pset-slots-quot set-slots[ "]" parse-until ;
! PARSER: pmemo-block MEMO[ "]" parse-until ;

! words{
! PARSER: parray { "}" parse-until ;
! PARSER: pvector V{ "}" parse-until ;
! PARSER: pbitset ?{ "}" parse-until ;
! PARSER: peval-dollar-array ${ "}" parse-until ; ! going away
! PARSER: pbyte-array B{ "}" parse-until ;
! PARSER: pbyte-vector BV{ "}" parse-until ;
! PARSER: phashtable H{ "}" parse-until ;
! PARSER: phash-set HS{ "}" parse-until ;
! PARSER: ptuple-literal T{ existing-class "}" parse-until ;
! PARSER: pcallstack-literal CS{ "}" parse-until ;
! PARSER: pcomplex-literal C{ "}" parse-until ;
! PARSER: pdlist-literal DL{ "}" parse-until ;
! PARSER: pwrapper-literal W{ "}" parse-until ;
! PARSER: pstruct-literal S{ "}" parse-until ;
! PARSER: pidentity-hash-set IHS{ "}" parse-until ;
! PARSER: pidentity-hashtable IH{ "}" parse-until ;
! PARSER: pset-array set{ "}" parse-until ;
! PARSER: pget-array get{ "}" parse-until ;
! PARSER: pslots-array slots{ "}" parse-until ;
! PARSER: pset-slots-array set-slots{ "}" parse-until ;
! PARSER: pcopy-slots-array copy-slots{ "}" parse-until ;
! PARSER: pflags flags{ "}" parse-until ;
! PARSER: punion-array union{ "}" parse-until ;
! PARSER: pintersection-array intersection{ "}" parse-until ;
! PARSER: pmaybe maybe{ "}" parse-until ;
! PARSER: pnot not{ "}" parse-until ;
! PARSER: pc-array c-array{ "}" parse-until ;

! PARSER: pshaped-array sa{ "}" parse-until ;
! PARSER: pavl AVL{ "}" parse-until ;
! PARSER: psplay SPLAY{ "}" parse-until ;
! PARSER: ptree TREE{ "}" parse-until ;
! PARSER: psuffix-array SA{ "}" parse-until ;
! PARSER: pvalist VA{ "}" parse-until ;
! PARSER: pvlist VL{ "}" parse-until ;
! PARSER: pnumber-hash-set NHS{ "}" parse-until ;
! PARSER: pnumber-hashtable NH{ "}" parse-until ;
! PARSER: pnibble-array N{ "}" parse-until ;
! PARSER: ppersistent-hashtable PH{ "}" parse-until ;
! PARSER: ppersistent-vector PV{ "}" parse-until ;
! PARSER: psequence-hash-set SHS{ "}" parse-until ;
! PARSER: psequence-hashtable SH{ "}" parse-until ;
! PARSER: pquote-word qw{ "}" parse-until ;
! PARSER: pbit-vector ?V{ "}" parse-until ;
! PARSER: ppoker-hand HAND{ "}" parse-until ;
! PARSER: phex-array HEX{ "}" parse-until ;

! words(
! PARSER: psignature ( ")" parguments typed-raw-until ;
! PARSER: pexecute-parens execute( (parse-psignature) ;
! PARSER: pcall-parens call( (parse-psignature) ;
! PARSER: peval-parens eval( (parse-psignature) ;
! PARSER: pdata-map-parens data-map( (parse-psignature) ;
! PARSER: pdata-map!-parens data-map!( (parse-psignature) ;
! PARSER: pshuffle-parens shuffle( (parse-psignature) ;



! END OF GO AWAY

PARSER: pcomment ! readln ;
PARSER: pshell-comment #! readln ;
: parse-pnested-comment' ( level -- )
    raw dup object>> {
        { [ dup "(*" = ] [ drop , 1 + parse-pnested-comment' ] }
        { [ dup "*)" = ] [ drop ptext doc-become , 1 - dup zero? [ drop ] [ parse-pnested-comment' ] if ] }
        { [ dup f = ] [ "*)" expected ] } ! failed
        [ drop , parse-pnested-comment' ]
    } cond ;
PARSER: pnested-comment (* [ 1 parse-pnested-comment' ] { } make ;

! BEGIN REGULAR WORDS
! Single token words
PARSER: pprivate-begin <PRIVATE ;
PARSER: pprivate-end PRIVATE> ;
PARSER: BAD-ALIEN BAD-ALIEN ;
PARSER: pdelimiter delimiter ;
PARSER: pdeprecated deprecated ;
PARSER: pf f ;
PARSER: pfinal final ;
PARSER: pflushable flushable ;
PARSER: pfoldable foldable ;
PARSER: pinline inline ;
PARSER: precursive recursive ;
PARSER: pd-register D ;
PARSER: pr-register R ;
PARSER: pbreakpoint B ;
PARSER: call-next-method call-next-method ;
PARSER: pno-compile no-compile ;
PARSER: pcycles cycles ;
PARSER: popcode opcode ;
PARSER: pspecialized specialized ;
PARSER: pgb GB ;

! Single token parsers that need rename (?)
PARSER: pin IN: token ;
PARSER: puse USE: token ;
PARSER: punuse UNUSE: token ;
PARSER: pfrom FROM: token "=>" expect ";" raw-until ;
PARSER: pexclude EXCLUDE: token "=>" expect ";" raw-until ;
PARSER: prename RENAME: raw raw "=>" expect raw ;
PARSER: pqualified QUALIFIED: token ;
PARSER: pqualified-with QUALIFIED-WITH: token token ;
PARSER: pforget FORGET: token ;

PARSER: pselector SELECTOR: token ; ! Smalltalk
PARSER: pstorage STORAGE: token ; ! units

! Nice, regular uppercase read til ; parsers
PARSER: pusing USING: ";" parse-until ;
PARSER: psyntax-word SYNTAX: raw body ; ! needs \
PARSER: pparser PARSER: raw raw body ; ! needs \

! Upper case but not explicit end
PARSER: pc-function FUNCTION: token new-word parse ;
PARSER: pfunction-alias FUNCTION-ALIAS: token token new-word parse ;
PARSER: px-function X-FUNCTION: token new-word parse ;
PARSER: pgl-function GL-FUNCTION: token new-word parse parse ;
PARSER: pcuda-function CUDA-FUNCTION: new-word parse ; ! no return value
PARSER: pcuda-global CUDA-GLOBAL: new-word ;
PARSER: pcuda-library CUDA-LIBRARY: new-word existing-class token ; ! XXX: token might have spaces...
PARSER: pc-callback CALLBACK: token token parse ;
PARSER: psubroutine SUBROUTINE: token parse ;


! WEIRD
! words[ funky
PARSER: plet-block [let "]" parse-until ;
PARSER: pinterpolate I[ "]I" multiline-string-until ;
PARSER: pxml-bracket [XML "XML]" multiline-string-until ;
PARSER: pinfix [infix "infix]" multiline-string-until ;
PARSER: pmorse [MORSE "MORSE]" multiline-string-until ;
PARSER: pebnf-bracket [EBNF token "EBNF]" multiline-string-until ; ! going away
PARSER: pebnf-acute <EBNF token "EBNF>" multiline-string-until ; ! going away
PARSER: pliterate <LITERATE "LITERATE>" multiline-string-until ;
PARSER: pxml-acute <XML "XML>" multiline-string-until ;


! words@
PARSER: pstruct-literal-at S@ token parse ; ! [[ ]]
PARSER: c-array@ c-array@ parse parse parse ; ! [[ ]]


! words:

PARSER: pfunction : new-word parse body ;
PARSER: pfunction-locals :: new-word parse body ;
PARSER: palias ALIAS: raw raw ;
PARSER: ptyped TYPED: new-word parse body ;
PARSER: ptyped-locals TYPED:: new-word parse body ;
PARSER: pmemo MEMO: new-word parse body ;
PARSER: pmemo-locals MEMO:: new-word parse body ;
PARSER: pidentity-memo IDENTITY-MEMO: new-word parse body ;
PARSER: pidentity-memo-locals IDENTITY-MEMO:: new-word parse body ;
PARSER: pmacro MACRO: new-word parse body ;
PARSER: pmacro-locals MACRO:: new-word parse body ;
PARSER: ppeg PEG: new-word parse body ;
PARSER: pdescriptive DESCRIPTIVE: new-word parse body ;
PARSER: pdescriptive-locals DESCRIPTIVE:: new-word parse body ;
PARSER: pconstructor-new CONSTRUCTOR: token token parse body ;
PARSER: pprimitive PRIMITIVE: new-word parse ;
PARSER: pfunctor FUNCTOR: token parse ";FUNCTOR" parse-until ;
PARSER: pfunctor-syntax FUNCTOR-SYNTAX: token body ;
PARSER: pgeneric GENERIC: new-class parse ;
PARSER: pgeneric# GENERIC# new-class token parse ;
PARSER: phook HOOK: new-class existing-word parse ;
PARSER: pmethod M: parse existing-word body ;
PARSER: pmethod-locals M:: parse existing-word body ;
PARSER: pmath MATH: new-word parse ;
PARSER: ppair-generic PAIR-GENERIC: new-class parse ;
PARSER: ppair-m PAIR-M: existing-class existing-class existing-word body ;
PARSER: ptags TAGS: new-word parse ;
PARSER: ptag TAG: token existing-word body ;
PARSER: prule RULE: new-word ";" raw-until ;
PARSER: proman ROMAN: token ;
PARSER: proman-op ROMAN-OP: raw parse ;
PARSER: plazy LAZY: new-word parse body ;
PARSER: pinfix-locals INFIX:: new-word parse body ;


PARSER: pconstant CONSTANT: token parse ;
PARSER: psymbol SYMBOL: token ;
PARSER: psymbols SYMBOLS: ";" raw-until ;
PARSER: ppostpone POSTPONE: raw ;
PARSER: pdefer DEFER: token ;
PARSER: pchar CHAR: raw ;
PARSER: palien ALIEN: token ;

PARSER: ptuple TUPLE: new-class body ;
PARSER: pstruct STRUCT: new-class body ;
PARSER: ppacked-struct PACKED-STRUCT: new-class body ;
PARSER: ple-packed-struct LE-PACKED-STRUCT: new-class body ;
PARSER: pbe-packed-struct BE-PACKED-STRUCT: new-class body ;
PARSER: ple-struct LE-STRUCT: new-class body ;
PARSER: pbe-struct BE-STRUCT: new-class body ;
PARSER: punion-struct UNION-STRUCT: new-class body ;
PARSER: perror ERROR: new-class body ;
PARSER: pslot SLOT: token ;
PARSER: pconstructor C: token token ;

PARSER: pcom-interface COM-INTERFACE: existing-word new-word parse ";" parse-until ;
PARSER: ptypedef TYPEDEF: token token ;
PARSER: plibrary LIBRARY: token ;
PARSER: pc-type C-TYPE: token ;
PARSER: pc-global C-GLOBAL: token token ;
PARSER: phints HINTS: parse body ;
PARSER: pbuiltin BUILTIN: existing-class body ;
PARSER: pmain MAIN: existing-word ;


PARSER: pdestructor DESTRUCTOR: existing-word ;
PARSER: ppredicate PREDICATE: new-class "<" expect parse body ;
PARSER: pmixin MIXIN: new-class ;
PARSER: pinstance INSTANCE: existing-class existing-class ;
PARSER: psingleton SINGLETON: new-class ;
PARSER: psingletons SINGLETONS: body ;
PARSER: pimport IMPORT: token ;
PARSER: pimports IMPORTS: ";" raw-until ;
PARSER: pspecial-object SPECIAL-OBJECT: token parse ;
PARSER: punion UNION: new-class body ;
PARSER: pintersection INTERSECTION: token body ;
PARSER: punicode-category CATEGORY: token token ;
PARSER: punicode-category-not CATEGORY-NOT: token token ;

PARSER: pspecialized-array SPECIALIZED-ARRAY: token ;
PARSER: pspecialized-arrays SPECIALIZED-ARRAYS: ";" raw-until ;
PARSER: pspecialized-vector SPECIALIZED-VECTOR: token ;
PARSER: pspecialized-vectors SPECIALIZED-VECTORS: ";" raw-until ;
PARSER: pvectored-struct VECTORED-STRUCT: existing-class ;

PARSER: pglsl-shader GLSL-SHADER: token token "\n;" multiline-string-until ;
PARSER: pglsl-program GLSL-PROGRAM: token body ;
PARSER: puniform-tuple UNIFORM-TUPLE: token body ;
PARSER: pebnf EBNF: token ";EBNF" multiline-string-until ;

PARSER: ptest TEST: token ;
PARSER: pregisters REGISTERS: body ;
PARSER: phi-registers HI-REGISTERS: body ;
PARSER: pcolor COLOR: token ;
PARSER: phexcolor HEXCOLOR: token ;
PARSER: pflexhexcolor FLEXHEXCOLOR: token ;

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
PARSER: phelp HELP: raw body ;
PARSER: pname NAME: token token ;
PARSER: ptr TR: token body ;

PARSER: pbackward-analysis BACKWARD-ANALYSIS: token ;
PARSER: pforward-analysis FORWARD-ANALYSIS: token ;
PARSER: pregister REGISTER: token ;

PARSER: plog LOG: token token ;
PARSER: pnan NAN: token ;
PARSER: pbroadcast BROADCAST: existing-word existing-class parse ;
PARSER: pconsult CONSULT: new-word existing-class body ;

PARSER: pmirc IRC: token parse ";" raw-until ;
PARSER: pmain-window MAIN-WINDOW: token parse body ;
PARSER: pgame GAME: token parse body ;
PARSER: psolution SOLUTION: token ;

PARSER: p8-bit 8-BIT: token token token ;
PARSER: peuc EUC: new-class parse ;

PARSER: plong-string STRING: token "\n;" multiline-string-until ;
PARSER: pbreakpoint-parse-time B: raw ; ! going away

PARSER: picon ICON: new-word token ;
PARSER: pmatch-vars MATCH-VARS: body ;
PARSER: ppixel-format-attribute-table PIXEL-FORMAT-ATTRIBUTE-TABLE: new-word parse parse ;
PARSER: prect RECT: parse parse ;


PARSER: c-global-literal &: token ;

PARSER: pslot-constructor SLOT-CONSTRUCTOR: token ;
PARSER: pslot-protocol SLOT-PROTOCOL: ";" raw-until ;

PARSER: ptip TIP: ";" parse-until ;
PARSER: ptokenizer TOKENIZER: existing-word ; ! hmm
PARSER: px509 X509_V_: new-word token ;

PARSER: prole ROLE: ";" raw-until ;
PARSER: prole-tuple ROLE-TUPLE: ";" raw-until ;
PARSER: pvariant VARIANT: ";" raw-until ;
PARSER: pvariant-member VARIANT-MEMBER: ";" raw-until ;

PARSER: pd D: token ;
PARSER: pdecimal DECIMAL: token ;

PARSER: pafter AFTER: existing-class existing-word body ;
PARSER: pbefore BEFORE: existing-class existing-word body ;
PARSER: papplescript APPLESCRIPT: new-word ";APPLESCRIPT" multiline-string-until ;
PARSER: pchloe CHLOE: new-word body ;
PARSER: pcomponent COMPONENT: token ;
PARSER: pderivative DERIVATIVE: existing-word body ;

PARSER: ptuple-array TUPLE-ARRAY: token ;
PARSER: pglsl-shader-file GLSL-SHADER-FILE: new-word existing-class parse ;

! gobject-instrospection
PARSER: pgif GIR: token ;
PARSER: pforeign-atomic-type FOREIGN-ATOMIC-TYPE: token token ;
PARSER: pforeign-enum-type FOREIGN-ENUM-TYPE: token token ;
PARSER: pforeign-record-type FOREIGN-RECORD-TYPE: token token ;
PARSER: pimplement-structs IMPLEMENT-STRUCTS: ";" raw-until ;


PARSER: pholiday HOLIDAY: new-word body ;
PARSER: pholiday-name HOLIDAY-NAME: existing-word existing-class parse ;


PARSER: pisntruction INSTRUCTION: ";" raw-until ;
PARSER: ppool POOL: existing-class token ;

PARSER: pmdbtuple MDBTUPLE: ";" parse-until ;

PARSER: ppy-from PY-FROM: new-word "=>" expect ";" parse-until ;
PARSER: ppy-methods PY-METHODS: new-word "=>" expect ";" parse-until ;
PARSER: ppy-qualified-from PY-QUALIFIED-FROM: new-word "=>" expect ";" parse-until ;
PARSER: prenaming RENAMING: new-word parse parse parse ;
PARSER: proll ROLL: token ;


PARSER: psingletons-union SINGLETONS-UNION: new-class ";" parse-until ;
! slides
PARSER: pstrip-tease STRIP-TEASE: ";" parse-until ;
PARSER: puse-rev USE-REV: token token ;
PARSER: pvertext-format VERTEX-FORMAT: new-word ";" parse-until ;
PARSER: pvertext-struct VERTEX-STRUCT: token token ;
PARSER: pxkcd XKCD: token ;
PARSER: pxml-error XML-ERROR: new-class ";" raw-until ;
PARSER: pxml-ns XML-NS: new-word token ;
PARSER: pfeedback-format feedback-format: token ;
PARSER: pgeometry-shader-vertices-out geometry-shader-vertices-out: parse ;
! funky

: parse-bind ( -- seq )
    raw dup dup [ object>> ] when "(" = [
        ")" raw-until 3array
    ] when ;
PARSER: pbind :> parse-bind ;

! =================================================
! words\
PARSER: pescaped \ raw ;
PARSER: pmethod-literal M\ token token ;

! funky readahead one
PARSER: peval-dollar $ parse ;

! singleton parsing words

! Cocoa
PARSER: pcfstring CFSTRING: new-word parse ;
! PARSER: pclass CLASS: new-class "<" expect existing-class parse ;
! PARSER: pcocoa-method METHOD: "[" parse-until "]" parse-until ;
PARSER: pframework FRAMEWORK: parse ;
PARSER: SEL: SEL: token ;
! PARSER: pcocoa-selector -> token ;
! PARSER: psuper-selector SUPER-> token ;

PARSER: pbacktick ` "`" multiline-string-until ;

/*
parsers get-global keys
all-words [ "syntax" word-prop ] filter
[ name>> ] map swap diff
natural-sort
[ . ] each

"%>"
*/
