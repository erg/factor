! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays io kernel modern.quick-parser multiline ;
IN: modern.syntax
QUALIFIED: sequences
QUALIFIED: strings

! Test cases:
! ALIAS: foo{ bar{
! HELP: foo{

! Can go away:
QPARSER: heredoc HEREDOC: token pick strings:>string multiline-string-until ;
! string literals
! QPARSER: " " ;
! QPARSER: P" P" ;
! QPARSER: URL" URL" ;
! QPARSER: SBUF" SBUF" ;
! QPARSER: DLL" DLL" ;

QPARSER: compilation-unit-begin << ; ! going away
QPARSER: compilation-unit-end >> ; ! going away

QPARSER: regexp-/ R/ "/" multiline-string-until ;
QPARSER: regexp-# R# "#" multiline-string-until ;
QPARSER: regexp-' R' "'" multiline-string-until ;
QPARSER: regexp-( R( "(" multiline-string-until ;
QPARSER: regexp-@ R@ "@" multiline-string-until ;
QPARSER: regexp-` R` "`" multiline-string-until ;
QPARSER: regexp-| R| "|" multiline-string-until ;
QPARSER: regexp-! R! "!" multiline-string-until ;

! words[
! QPARSER: block [ "]" parse-until ;
! QPARSER: fry '[ "]" parse-until ;
! QPARSER: block-eval $[ "]" parse-until ;
! QPARSER: set-quot set[ "]" parse-until ;
! QPARSER: get-quot get[ "]" parse-until ;
! QPARSER: slots-quot slots[ "]" parse-until ;
! QPARSER: set-slots-quot set-slots[ "]" parse-until ;
! QPARSER: memo-block MEMO[ "]" parse-until ;

QPARSER: block-locals [| "]" parse-until ;
QPARSER: pipe-separator | ;

! words{
! QPARSER: array { "}" parse-until ;
! QPARSER: vector V{ "}" parse-until ;
! QPARSER: bitset ?{ "}" parse-until ;
! QPARSER: eval-dollar-array ${ "}" parse-until ; ! going away
! QPARSER: byte-array B{ "}" parse-until ;
! QPARSER: byte-vector BV{ "}" parse-until ;
! QPARSER: hashtable H{ "}" parse-until ;
! QPARSER: hash-set HS{ "}" parse-until ;
! QPARSER: tuple-literal T{ existing-class "}" parse-until ;
! QPARSER: callstack-literal CS{ "}" parse-until ;
! QPARSER: complex-literal C{ "}" parse-until ;
! QPARSER: dlist-literal DL{ "}" parse-until ;
! QPARSER: wrapper-literal W{ "}" parse-until ;
! QPARSER: struct-literal S{ "}" parse-until ;
! QPARSER: identity-hash-set IHS{ "}" parse-until ;
! QPARSER: identity-hashtable IH{ "}" parse-until ;
! QPARSER: set-array set{ "}" parse-until ;
! QPARSER: get-array get{ "}" parse-until ;
! QPARSER: slots-array slots{ "}" parse-until ;
! QPARSER: set-slots-array set-slots{ "}" parse-until ;
! QPARSER: copy-slots-array copy-slots{ "}" parse-until ;
! QPARSER: flags flags{ "}" parse-until ;
! QPARSER: union-array union{ "}" parse-until ;
! QPARSER: intersection-array intersection{ "}" parse-until ;
! QPARSER: maybe maybe{ "}" parse-until ;
! QPARSER: not not{ "}" parse-until ;
! QPARSER: c-array c-array{ "}" parse-until ;

! QPARSER: shaped-array sa{ "}" parse-until ;
! QPARSER: avl AVL{ "}" parse-until ;
! QPARSER: splay SPLAY{ "}" parse-until ;
! QPARSER: tree TREE{ "}" parse-until ;
! QPARSER: suffix-array SA{ "}" parse-until ;
! QPARSER: valist VA{ "}" parse-until ;
! QPARSER: vlist VL{ "}" parse-until ;
! QPARSER: number-hash-set NHS{ "}" parse-until ;
! QPARSER: number-hashtable NH{ "}" parse-until ;
! QPARSER: nibble-array N{ "}" parse-until ;
! QPARSER: persistent-hashtable PH{ "}" parse-until ;
! QPARSER: persistent-vector PV{ "}" parse-until ;
! QPARSER: sequence-hash-set SHS{ "}" parse-until ;
! QPARSER: sequence-hashtable SH{ "}" parse-until ;
! QPARSER: quote-word qw{ "}" parse-until ;
! QPARSER: bit-vector ?V{ "}" parse-until ;
! QPARSER: poker-hand HAND{ "}" parse-until ;
! QPARSER: hex-array HEX{ "}" parse-until ;

! words(
! QPARSER: signature ( ")" parguments typed-raw-until ;
! QPARSER: execute-parens execute( (parse-psignature) ;
! QPARSER: call-parens call( (parse-psignature) ;
! QPARSER: eval-parens eval( (parse-psignature) ;
! QPARSER: data-map-parens data-map( (parse-psignature) ;
! QPARSER: data-map!-parens data-map!( (parse-psignature) ;
! QPARSER: shuffle-parens shuffle( (parse-psignature) ;

! END OF GO AWAY


! QPARSER: comment ! skip-til-eol ;
! QPARSER: shell-comment #! skip-til-eol ;
QPARSER: c-comment /* "*/" multiline-string-until ;

! BEGIN REGULAR WORDS
! Single token words, probably don't need to be parsing words except for f maybe
QPARSER: f f ;
QPARSER: private-begin <PRIVATE ;
QPARSER: private-end PRIVATE> ;
QPARSER: BAD-ALIEN BAD-ALIEN ; ! alien.syntax
QPARSER: delimiter delimiter ;
QPARSER: deprecated deprecated ;
QPARSER: final final ;
QPARSER: flushable flushable ;
QPARSER: foldable foldable ;
QPARSER: inline inline ;
QPARSER: recursive recursive ;
QPARSER: breakpoint B ;
QPARSER: call-next-method call-next-method ;
QPARSER: no-compile no-compile ; ! extra/benchmark/raytracer-simd/raytracer-simd.factor
! QPARSER: specialized specialized ; ! what is this? gone?

! Compiler
QPARSER: d-register D: token ;
QPARSER: r-register R: token ;

! opengl break
QPARSER: gb GB ;

! XXX: cpu.8080
QPARSER: instruction INSTRUCTION: ";" raw-until ;
QPARSER: cycles cycles: token ;
QPARSER: opcode opcode: token ;

! Single token parsers that need rename (?)
QPARSER: in IN: token ;
QPARSER: use USE: token ;

QPARSER: unuse UNUSE: token ; ! XXX: remove this
QPARSER: exclude EXCLUDE: token "=>" expect ";" raw-until ; ! XXX: remove
QPARSER: rename RENAME: raw raw "=>" expect raw ; ! XXX: remove
QPARSER: from FROM: token "=>" expect ";" raw-until ; ! XXX: remove
QPARSER: qualified QUALIFIED: token ; ! XXX: make implicit
QPARSER: qualified-with QUALIFIED-WITH: token token ; ! XXX: remove

QPARSER: forget FORGET: token ; ! repl only (?)

QPARSER: guid GUID: raw ;

QPARSER: selector SELECTOR: token ; ! Smalltalk
QPARSER: storage STORAGE: token ; ! units

! Nice, regular uppercase read til ; parsers
QPARSER: using USING: ";" parse-until ;
QPARSER: syntax-word SYNTAX: raw body ; ! needs \
QPARSER: parser QPARSER: raw raw body ; ! needs \

! Upper case but not explicit end
QPARSER: c-function FUNCTION: token new-word parse ;
QPARSER: function-alias FUNCTION-ALIAS: token token new-word parse ;
QPARSER: x-function X-FUNCTION: token new-word parse ;
QPARSER: gl-function GL-FUNCTION: token new-word parse parse ;
QPARSER: cuda-function CUDA-FUNCTION: new-word parse ; ! no return value
QPARSER: cuda-global CUDA-GLOBAL: new-word ;
QPARSER: cuda-library CUDA-LIBRARY: new-word existing-class token ; ! XXX: token might have spaces...
QPARSER: c-callback CALLBACK: token token parse ;
QPARSER: subroutine SUBROUTINE: token parse ;


! WEIRD
! words[ funky
QPARSER: let-block [let "]" parse-until ;
QPARSER: interpolate [I "I]" multiline-string-until ;
QPARSER: xml-bracket [XML "XML]" multiline-string-until ;
QPARSER: infix [infix "infix]" multiline-string-until ;
QPARSER: morse [MORSE "MORSE]" multiline-string-until ;
QPARSER: ebnf-bracket [EBNF token "EBNF]" multiline-string-until ; ! going away
QPARSER: ebnf-acute <EBNF token "EBNF>" multiline-string-until ; ! going away
QPARSER: literate <LITERATE "LITERATE>" multiline-string-until ;
QPARSER: xml-acute <XML "XML>" multiline-string-until ;

QPARSER: backtick ` "`" multiline-string-until ;
QPARSER: applescript APPLESCRIPT: new-word ";APPLESCRIPT" multiline-string-until ;
QPARSER: long-string STRING: token "\n;" multiline-string-until ;
QPARSER: glsl-shader GLSL-SHADER: token token "\n;" multiline-string-until ;
QPARSER: ebnf EBNF: token ";EBNF" multiline-string-until ;


! words@
QPARSER: struct-literal-at S@ token parse ; ! [[ ]]
QPARSER: c-array@ c-array@ parse parse parse ; ! [[ ]]


! words:

QPARSER: function : new-word parse body ;
QPARSER: function-locals :: new-word parse body ;
QPARSER: alias ALIAS: raw raw ;
QPARSER: typed TYPED: new-word parse body ;
QPARSER: typed-locals TYPED:: new-word parse body ;
QPARSER: memo MEMO: new-word parse body ;
QPARSER: memo-locals MEMO:: new-word parse body ;
QPARSER: identity-memo IDENTITY-MEMO: new-word parse body ;
QPARSER: identity-memo-locals IDENTITY-MEMO:: new-word parse body ;
QPARSER: macro MACRO: new-word parse body ;
QPARSER: macro-locals MACRO:: new-word parse body ;
QPARSER: peg PEG: new-word parse body ;
QPARSER: descriptive DESCRIPTIVE: new-word parse body ;
QPARSER: descriptive-locals DESCRIPTIVE:: new-word parse body ;
QPARSER: constructor-new CONSTRUCTOR: token token parse body ;
QPARSER: primitive PRIMITIVE: new-word parse ;
! XXX: new def, can't use yet
! QPARSER: functor FUNCTOR: token parse ";FUNCTOR" parse-until ;
QPARSER: functor FUNCTOR: token ";FUNCTOR" multiline-string-until ;
QPARSER: functor-syntax FUNCTOR-SYNTAX: token body ;
QPARSER: generic GENERIC: new-class parse ;
QPARSER: generic# GENERIC# new-class token parse ;
QPARSER: hook HOOK: new-class existing-word parse ;
QPARSER: method M: parse existing-word body ;
QPARSER: method-locals M:: parse existing-word body ;
QPARSER: math MATH: new-word parse ;
QPARSER: pair-generic PAIR-GENERIC: new-class parse ;
QPARSER: pair-m PAIR-M: existing-class existing-class existing-word body ;
QPARSER: tags TAGS: new-word parse ;
QPARSER: tag TAG: token existing-word body ;
QPARSER: rule RULE: new-word ";" raw-until ;
QPARSER: roman ROMAN: token ;
QPARSER: roman-op ROMAN-OP: raw parse ;
QPARSER: lazy LAZY: new-word parse body ;
QPARSER: infix-locals INFIX:: new-word parse body ;


QPARSER: constant CONSTANT: token parse ;
QPARSER: symbol SYMBOL: token ;
QPARSER: symbols SYMBOLS: ";" raw-until ;
QPARSER: postpone POSTPONE: raw ;
QPARSER: defer DEFER: token ;
QPARSER: char CHAR: raw ;
QPARSER: alien ALIEN: token ;

QPARSER: tuple TUPLE: new-class body ;
QPARSER: struct STRUCT: new-class body ;
QPARSER: packed-struct PACKED-STRUCT: new-class body ;
QPARSER: le-packed-struct LE-PACKED-STRUCT: new-class body ;
QPARSER: be-packed-struct BE-PACKED-STRUCT: new-class body ;
QPARSER: le-struct LE-STRUCT: new-class body ;
QPARSER: be-struct BE-STRUCT: new-class body ;
QPARSER: union-struct UNION-STRUCT: new-class body ;
QPARSER: error ERROR: new-class body ;
QPARSER: slot SLOT: token ;
QPARSER: constructor C: token token ;

QPARSER: com-interface COM-INTERFACE: existing-word new-word parse ";" parse-until ;
QPARSER: typedef TYPEDEF: token token ;
QPARSER: library LIBRARY: token ;
QPARSER: c-type C-TYPE: token ;
QPARSER: c-global C-GLOBAL: token token ;
QPARSER: hints HINTS: parse body ;
QPARSER: builtin BUILTIN: existing-class body ;
QPARSER: main MAIN: existing-word ;


QPARSER: destructor DESTRUCTOR: existing-word ;
QPARSER: predicate PREDICATE: new-class "<" expect parse body ;
QPARSER: mixin MIXIN: new-class ;
QPARSER: instance INSTANCE: existing-class existing-class ;
QPARSER: singleton SINGLETON: new-class ;
QPARSER: singletons SINGLETONS: body ;
QPARSER: import IMPORT: token ;
QPARSER: imports IMPORTS: ";" raw-until ;
QPARSER: special-object SPECIAL-OBJECT: token parse ;
QPARSER: union UNION: new-class body ;
QPARSER: intersection INTERSECTION: token body ;
QPARSER: unicode-category CATEGORY: token body ;
QPARSER: unicode-category-not CATEGORY-NOT: token body ;

QPARSER: specialized-array SPECIALIZED-ARRAY: token ;
QPARSER: specialized-arrays SPECIALIZED-ARRAYS: ";" raw-until ;
QPARSER: specialized-vector SPECIALIZED-VECTOR: token ;
QPARSER: specialized-vectors SPECIALIZED-VECTORS: ";" raw-until ;
QPARSER: vectored-struct VECTORED-STRUCT: existing-class ;

QPARSER: glsl-program GLSL-PROGRAM: token body ;
QPARSER: uniform-tuple UNIFORM-TUPLE: token body ;

QPARSER: test TEST: token ;
QPARSER: registers REGISTERS: body ;
QPARSER: hi-registers HI-REGISTERS: body ;
QPARSER: color COLOR: token ;
QPARSER: hexcolor HEXCOLOR: token ;
QPARSER: flexhexcolor FLEXHEXCOLOR: token ;

QPARSER: about ABOUT: token ;
QPARSER: article ARTICLE: token body ;
QPARSER: rotocol PROTOCOL: token body ;

QPARSER: insn INSN: new-word body ;
QPARSER: vreg-insn VREG-INSN: token body ;
QPARSER: flushable-insn FLUSHABLE-INSN: new-word body ;
QPARSER: foldable-insn FOLDABLE-INSN: new-word body ;

QPARSER: codegen CODEGEN: token token ;
QPARSER: conditional CONDITIONAL: token token ;
QPARSER: simd-128 SIMD-128: token ;
QPARSER: simd-128-cord SIMD-128-CORD: token token ;
QPARSER: simd-instrinsic SIMD-INTRINSIC: token body ;
QPARSER: simd-instrinsic-locals SIMD-INTRINSIC:: token body ;
QPARSER: enum ENUM: token body ;
QPARSER: pointer pointer: token ;
QPARSER: help HELP: raw body ;
QPARSER: name NAME: token token ;
QPARSER: tr TR: token body ;

QPARSER: backward-analysis BACKWARD-ANALYSIS: token ;
QPARSER: forward-analysis FORWARD-ANALYSIS: token ;
QPARSER: register REGISTER: token ;

QPARSER: log LOG: token token ;
QPARSER: nan NAN: token ;
QPARSER: broadcast BROADCAST: existing-word existing-class parse ;
QPARSER: consult CONSULT: new-word existing-class body ;

QPARSER: mirc IRC: token parse ";" raw-until ;
QPARSER: main-window MAIN-WINDOW: token parse body ;
QPARSER: game GAME: token parse body ;
QPARSER: solution SOLUTION: token ;

QPARSER: 8-bit 8-BIT: token token token ;
QPARSER: euc EUC: new-class parse ;

QPARSER: breakpoint-parse-time B: raw ; ! going away

QPARSER: icon ICON: new-word token ;
QPARSER: match-vars MATCH-VARS: body ;
QPARSER: pixel-format-attribute-table PIXEL-FORMAT-ATTRIBUTE-TABLE: new-word parse parse ;
QPARSER: rect RECT: parse parse ;


QPARSER: c-global-literal &: token ;

QPARSER: slot-constructor SLOT-CONSTRUCTOR: token ;
QPARSER: slot-protocol SLOT-PROTOCOL: ";" raw-until ;

QPARSER: tip TIP: ";" parse-until ;
QPARSER: tokenizer TOKENIZER: existing-word ; ! hmm
QPARSER: x509 X509_V_: new-word token ;

QPARSER: role ROLE: ";" raw-until ;
QPARSER: role-tuple ROLE-TUPLE: ";" raw-until ;
QPARSER: variant VARIANT: ";" raw-until ;
QPARSER: variant-member VARIANT-MEMBER: ";" raw-until ;

QPARSER: decimal DECIMAL: token ;

QPARSER: after AFTER: existing-class existing-word body ;
QPARSER: before BEFORE: existing-class existing-word body ;
QPARSER: chloe CHLOE: new-word body ;
QPARSER: component COMPONENT: token ;
QPARSER: derivative DERIVATIVE: existing-word body ;

QPARSER: tuple-array TUPLE-ARRAY: token ;
QPARSER: glsl-shader-file GLSL-SHADER-FILE: new-word existing-class parse ;

! gobject-instrospection
QPARSER: gif GIR: token ;
QPARSER: foreign-atomic-type FOREIGN-ATOMIC-TYPE: token token ;
QPARSER: foreign-enum-type FOREIGN-ENUM-TYPE: token token ;
QPARSER: foreign-record-type FOREIGN-RECORD-TYPE: token token ;
QPARSER: implement-structs IMPLEMENT-STRUCTS: ";" raw-until ;


QPARSER: holiday HOLIDAY: new-word body ;
QPARSER: holiday-name HOLIDAY-NAME: existing-word existing-class parse ;


QPARSER: pool POOL: existing-class token ;

QPARSER: mdbtuple MDBTUPLE: ";" parse-until ;

QPARSER: py-from PY-FROM: new-word "=>" expect ";" parse-until ;
QPARSER: py-methods PY-METHODS: new-word "=>" expect ";" parse-until ;
QPARSER: py-qualified-from PY-QUALIFIED-FROM: new-word "=>" expect ";" parse-until ;
QPARSER: renaming RENAMING: new-word parse parse parse ;
QPARSER: roll ROLL: token ;


QPARSER: singletons-union SINGLETONS-UNION: new-class ";" parse-until ;
! slides
QPARSER: strip-tease STRIP-TEASE: ";" parse-until ;
QPARSER: use-rev USE-REV: token token ;
QPARSER: vertext-format VERTEX-FORMAT: new-word ";" parse-until ;
QPARSER: vertext-struct VERTEX-STRUCT: token token ;
QPARSER: xkcd XKCD: token ;
QPARSER: xml-error XML-ERROR: new-class ";" raw-until ;
QPARSER: xml-ns XML-NS: new-word token ;
QPARSER: feedback-format feedback-format: token ;
QPARSER: geometry-shader-vertices-out geometry-shader-vertices-out: parse ;
! funky

: parse-bind ( n string -- seq n'/f string )
    raw pick "(" sequences:sequence= [
        ")" raw-until [ swap sequences:prefix ] 2dip
    ] when ;
QPARSER: bind :> parse-bind ;

! =================================================
! words\
QPARSER: escaped \ raw ;
QPARSER: method-literal M\ token token ;

! funky readahead one, need this for things like CONSTANT: foo $ init-foo
! XXX: Maybe call it $: instead.
QPARSER: eval-dollar $ parse ;

! singleton parsing words

! Cocoa
QPARSER: cfstring CFSTRING: new-word parse ;

QPARSER: class CLASS: new-class "<" expect ";" parse-until ;
QPARSER: cocoa-method METHOD: token ";" parse-until ;
QPARSER: cocoa-protocol COCOA-PROTOCOL: token ;

QPARSER: framework FRAMEWORK: parse ;
QPARSER: SEL: SEL: token ;
! QPARSER: cocoa-selector -> token ;
QPARSER: super-selector SUPER-> token ;


/*
parsers get-global keys
all-words [ "syntax" word-prop ] filter
[ name>> ] map swap diff
natural-sort
[ . ] each

"%>"
*/
