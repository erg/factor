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

! PARSER: psignature ( parse-open-signature ;
TUPLE: psignature < psequence ;

! Hacky, should be able to call parse-psignature with its "(" expect
: parse-entire-signature ( -- seq )
    [ "(" expect parse-open-signature ] output>array psignature boa ;

! string literals
! PARSER: " " ;
! PARSER: P" P" ;
! PARSER: URL" URL" ;
! PARSER: SBUF" SBUF" ;
! PARSER: DLL" DLL" ;
! exec"

! words[
PARSER: pblock [ "]" parse-until ;
PARSER: pfry '[ "]" parse-until ;
PARSER: pblock-eval $[ "]" parse-until ;
PARSER: pblock-locals [| "]" parse-until ;
PARSER: pset-quot set[ "]" parse-until ;
PARSER: pget-quot get[ "]" parse-until ;
PARSER: pslots-quot slots[ "]" parse-until ;
PARSER: pset-slots-quot set-slots[ "]" parse-until ;
PARSER: pmemo-block MEMO[ "]" parse-until ;

! words[ funky
PARSER: plet-block [let "]" parse-until ;
PARSER: pinterpolate I[ "]I" multiline-string-until ;
! "[MORSE"
! "[XML"
! "[infix"

! words{
PARSER: parray { "}" parse-until ;
PARSER: pvector V{ "}" parse-until ;
PARSER: pbitset ?{ "}" parse-until ;
PARSER: peval-dollar-array ${ "}" parse-until ; ! going away
PARSER: pbyte-array B{ "}" parse-until ;
PARSER: pbyte-vector BV{ "}" parse-until ;
PARSER: phashtable H{ "}" parse-until ;
PARSER: phash-set HS{ "}" parse-until ;
PARSER: ptuple-literal T{ existing-class "}" parse-until ;
PARSER: pcallstack-literal CS{ "}" parse-until ;
PARSER: pcomplex-literal C{ "}" parse-until ;
PARSER: pdlist-literal DL{ "}" parse-until ;
PARSER: pwrapper-literal W{ "}" parse-until ;
PARSER: pstruct-literal S{ "}" parse-until ;
PARSER: pidentity-hash-set IHS{ "}" parse-until ;
PARSER: pidentity-hashtable IH{ "}" parse-until ;
PARSER: pset-array set{ "}" parse-until ;
PARSER: pget-array get{ "}" parse-until ;
PARSER: pslots-array slots{ "}" parse-until ;
PARSER: pset-slots-array set-slots{ "}" parse-until ;
PARSER: pcopy-slots-array copy-slots{ "}" parse-until ;
PARSER: pflags flags{ "}" parse-until ;
PARSER: punion-array union{ "}" parse-until ;
PARSER: pintersection-array intersection{ "}" parse-until ;
PARSER: pmaybe maybe{ "}" parse-until ;
PARSER: pnot not{ "}" parse-until ;
PARSER: pc-array c-array{ "}" parse-until ;

PARSER: pshaped-array sa{ "}" parse-until ;
PARSER: pavl AVL{ "}" parse-until ;
PARSER: psplay SPLAY{ "}" parse-until ;
PARSER: ptree TREE{ "}" parse-until ;
PARSER: psuffix-array SA{ "}" parse-until ;
PARSER: pvalist VA{ "}" parse-until ;
PARSER: pvlist VL{ "}" parse-until ;
PARSER: pnumber-hash-set NHS{ "}" parse-until ;
PARSER: pnumber-hashtable NH{ "}" parse-until ;
PARSER: pnibble-array N{ "}" parse-until ;
PARSER: ppersistent-hashtable PH{ "}" parse-until ;
PARSER: ppersistent-vector PV{ "}" parse-until ;
PARSER: psequence-hash-set SHS{ "}" parse-until ;
PARSER: psequence-hashtable SH{ "}" parse-until ;
PARSER: pquote-word qw{ "}" parse-until ;
PARSER: pbit-vector ?V{ "}" parse-until ;
PARSER: ppoker-hand HAND{ "}" parse-until ;
PARSER: phex-array HEX{ "}" parse-until ;

! words@
PARSER: pstruct-literal-at S@ token parse ; ! [[ ]]
PARSER: c-array@ c-array@ parse parse parse ; ! [[ ]]

! words(
! PARSER: psignature ( ;
PARSER: pexecute-parens execute( parse-open-signature ;
PARSER: pcall-parens call( parse-open-signature ;
PARSER: peval-parens eval( parse-open-signature ;
PARSER: pdata-map-parens data-map( parse-open-signature ;
PARSER: pdata-map!-parens data-map!( parse-open-signature ;
PARSER: pshuffle-parens shuffle( ")" parse-until ;

! words:
PARSER: pfunction : new-identifier parse-entire-signature body ;
PARSER: pfunction-locals :: new-identifier parse-entire-signature body ;
PARSER: palias ALIAS: new-word existing-word ;
PARSER: ptyped TYPED: new-identifier parse-entire-signature body ;
PARSER: ptyped-locals TYPED:: new-identifier parse-entire-signature body ;
PARSER: pmemo MEMO: new-identifier parse-entire-signature body ;
PARSER: pmemo-locals MEMO:: new-identifier parse-entire-signature body ;
PARSER: pidentity-memo IDENTITY-MEMO: new-identifier parse-entire-signature body ;
PARSER: pmacro MACRO: new-identifier parse-entire-signature body ;
PARSER: pmacro-locals MACRO:: new-identifier parse-entire-signature body ;
PARSER: ppeg PEG: new-identifier parse-entire-signature body ;

PARSER: pconstant CONSTANT: token parse ;
PARSER: psymbol SYMBOL: token ;
PARSER: psymbols SYMBOLS: ";" raw-until ;
PARSER: ppostpone POSTPONE: raw ;
PARSER: pdefer DEFER: token ;
PARSER: pchar CHAR: raw ;
PARSER: palien ALIEN: token ;

PARSER: ptuple TUPLE: new-identifier body ;
PARSER: pstruct STRUCT: new-identifier body ;
PARSER: ppacked-struct PACKED-STRUCT: new-identifier body ;
PARSER: punion-struct UNION-STRUCT: new-identifier body ;
PARSER: perror ERROR: new-identifier body ;
PARSER: pslot SLOT: token ;
PARSER: pconstructor C: token token ;
PARSER: pconstructor-new CONSTRUCTOR: token token parse-entire-signature body ;
: c-arguments ( -- sep arguments sep ) "(" expect ")" raw-until ;
PARSER: pc-function FUNCTION: token new-identifier c-arguments ;
PARSER: pfunction-alias FUNCTION-ALIAS: token token new-identifier c-arguments ;
PARSER: px-function X-FUNCTION: token new-identifier c-arguments ;
PARSER: pgl-function GL-FUNCTION: token new-identifier parse c-arguments ;
PARSER: pcuda-function CUDA-FUNCTION: token new-identifier c-arguments ;
PARSER: pcuda-global CUDA-GLOBAL: new-word ;
PARSER: pcuda-library CUDA-LIBRARY: new-word existing-class token ; ! XXX: token might have spaces...
PARSER: pc-callback CALLBACK: token token c-arguments ;
PARSER: psubroutine SUBROUTINE: token c-arguments ;

PARSER: pcom-interface COM-INTERFACE: token new-word parse ";" parse-until ;
PARSER: ptypedef TYPEDEF: token token ;
PARSER: plibrary LIBRARY: token ;
PARSER: pc-type C-TYPE: token ;
PARSER: pc-global C-GLOBAL: token token ;
PARSER: phints HINTS: parse body ;
PARSER: pbuiltin BUILTIN: token body ;
PARSER: pprimitive PRIMITIVE: new-word parse-entire-signature ;
PARSER: pmain MAIN: existing-word ;

PARSER: pfunctor FUNCTOR: token parse-entire-signature ";FUNCTOR" parse-until ;
PARSER: pfunctor-syntax FUNCTOR-SYNTAX: token body ;

PARSER: pdestructor DESTRUCTOR: existing-word ;
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
PARSER: pspecialized-vector SPECIALIZED-VECTOR: token ;
PARSER: pspecialized-vectors SPECIALIZED-VECTORS: ";" raw-until ;
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
PARSER: phelp HELP: token body ;
PARSER: pname NAME: token token ;
PARSER: ptr TR: token body ;

PARSER: pbackward-analysis BACKWARD-ANALYSIS: token ;
PARSER: pforward-analysis FORWARD-ANALYSIS: token ;

PARSER: plog LOG: token token ;
PARSER: pnan NAN: token ;
PARSER: pbroadcast BROADCAST: existing-word existing-class parse ;
PARSER: pconsult CONSULT: new-word existing-class body ;

PARSER: pmirc IRC: token parse ";" raw-until ;
PARSER: pmain-window MAIN-WINDOW: token parse body ;
PARSER: pgame GAME: token parse body ;
PARSER: psolution SOLUTION: token ;
PARSER: p8-bit 8-BIT: token token token ;

PARSER: plong-string STRING: token "\n;" multiline-string-until ;
PARSER: pbreakpoint-parse-time B: raw ; ! going away

PARSER: picon ICON: new-word token ;
PARSER: pidentity-memo-locals IDENTITY-MEMO:: new-word parse-entire-signature body ;
PARSER: pmatch-vars MATCH-VARS: body ;
PARSER: ppixel-format-attribute-table PIXEL-FORMAT-ATTRIBUTE-TABLE: new-word parse parse ;
PARSER: prect RECT: parse parse ;


PARSER: c-global-literal &: token ;

PARSER: pslot-constructor SLOT-CONSTRUCTOR: token ;
PARSER: pslot-protocol SLOT-PROTOCOL: ";" raw-until ;

PARSER: ptip TIP: ";" parse-until ;
PARSER: ptokenizer TOKENIZER: existing-word ; ! hmm
PARSER: px509 X509_V_: new-word token ;

PARSER: ptags TAGS: new-word parse-entire-signature ;
PARSER: ptag TAG: token existing-word body ;

! funky

: parse-bind ( -- seq )
    raw dup dup [ object>> ] when "(" = [
        ")" raw-until 3array
    ] when ;
PARSER: pbind :> parse-bind ;

! words\
PARSER: pescaped \ raw ;
PARSER: pmethod-literal M\ token token ;

! funky readahead one
PARSER: peval-dollar $ parse ;

! singleton parsing words
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

! paired singleton parsing words
PARSER: pprivate-begin <PRIVATE ;
PARSER: pprivate-end PRIVATE> ;
PARSER: pcompilation-unit-begin << ; ! going away
PARSER: pcompilation-unit-end >> ; ! going away

! Cocoa
PARSER: pcfstring CFSTRING: new-word parse ;
! PARSER: pclass CLASS: new-class "<" expect existing-class parse ;
! PARSER: pcocoa-method METHOD: "[" parse-until "]" parse-until ;
PARSER: pframework FRAMEWORK: parse ;
PARSER: SEL: SEL: token ;
! PARSER: pcocoa-selector -> token ;
! PARSER: psuper-selector SUPER-> token ;

! funky
PARSER: pebnf-acute <EBNF token "EBNF>" multiline-string-until ; ! going away
PARSER: pebnf-bracket [EBNF token "EBNF]" multiline-string-until ; ! going away

! go away
: parse-pnested-comment' ( level -- )
    raw dup object>> {
        { [ dup "(*" = ] [ drop , 1 + parse-pnested-comment' ] }
        { [ dup "*)" = ] [ drop ptext pbecome , 1 - dup zero? [ drop ] [ parse-pnested-comment' ] if ] }
        { [ dup f = ] [ "*)" expected ] } ! failed
        [ drop , parse-pnested-comment' ]
    } cond ;
PARSER: pnested-comment (* [ 1 parse-pnested-comment' ] { } make ;

/*
"%\""
"%>"
".."
"..."

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
! PARSER: RESET RESET ;


parsers get-global keys
all-words [ "syntax" word-prop ] filter
[ name>> ] map swap diff
natural-sort
[ . ] each



"ESC"
"GB"

"<LITERATE"
"<XML"
"=>"

PARSER: pd D: token ;
PARSER: pdecimal DECIMAL: token ;

PARSER: pafter AFTER: existing-class existing-word body ;
PARSER: pbefore BEFORE: existing-class existing-word body ;
PARSER: papplescript APPLESCRIPT: scan-new-word ";APPLESCRIPT" multiline-string-until ;
PARSER: pchloe CHLOE: new-word body ;
PARSER: pcomponent COMPONENT: token ;
PARSER: pderivative DERIVATIVE: existing-word body ;
PARSER: pdescriptive DESCRIPTIVE:
PARSER: pdescriptive-locals DESCRIPTIVE::
PARSER: peuc EUC:
PARSER: pexec EXEC:
PARSER: pexec-locals EXEC::
PARSER: pfont FONT:
PARSER: pforeign-atomic-type FOREIGN-ATOMIC-TYPE:
PARSER: pforeign-enum-type FOREIGN-ENUM-TYPE:
PARSER: pforeign-record-type FOREIGN-RECORD-TYPE:
PARSER: pgif GIR:
PARSER: pglsl-shader-file GLSL-SHADER-FILE:
PARSER: pgml GML:
PARSER: pgml-locals GML::
PARSER: pholiday-name HOLIDAY-NAME:
PARSER: pholiday HOLIDAY:
PARSER: pimplement-structs IMPLEMENT-STRUCTS:
PARSER: pinfix-locals INFIX::
PARSER: pisntruction INSTRUCTION:
PARSER: plazy LAZY:
PARSER: ple-packed-struct LE-PACKED-STRUCT:
PARSER: pbe-packed-struct BE-PACKED-STRUCT:
PARSER: ple-struct LE-STRUCT:
PARSER: pbe-struct BE-STRUCT:
PARSER: plog-gml LOG-GML:
PARSER: pmdbtuple MDBTUPLE:
PARSER: ppair-generic PAIR-GENERIC:
PARSER: ppair-m PAIR-M:
PARSER: ppool POOL:
PARSER: ppy-from PY-FROM:
PARSER: ppy-methods PY-METHODS:
PARSER: ppy-qualified-from PY-QUALIFIED-FROM:
PARSER: pregister REGISTER:
PARSER: prenaming RENAMING:
PARSER: prole ROLE:
PARSER: proll ROLL:
PARSER: proman-op ROMAN-OP:
PARSER: proman ROMAN:
PARSER: prule RULE:
PARSER: pselector SELECTOR:
PARSER: psingletons-union SINGLETONS-UNION:
PARSER: pstorage STORAGE:
PARSER: pstored-tuple STORED-TUPLE:
PARSER: pstrip-tease STRIP-TEASE:
PARSER: ptuple-array TUPLE-ARRAY:
PARSER: puse-rev USE-REV:
PARSER: pvariant-member VARIANT-MEMBER:
PARSER: pvariant VARIANT:
PARSER: pvectored-struct VECTORED-STRUCT:
PARSER: pvertext-format VERTEX-FORMAT:
PARSER: pvertext-struct VERTEX-STRUCT:
PARSER: pxkcd XKCD:
PARSER: pxml-error XML-ERROR:
PARSER: pxml-ns XML-NS:
PARSER: pfeedback-format feedback-format:
PARSER: pgeometry-shader-vertices-out geometry-shader-vertices-out:
"`"
*/
