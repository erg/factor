! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: modern.quick-parser ;
IN: modern.factor

QPARSER: using USING: ";" parse-until ;
QPARSER: syntax-word SYNTAX: raw body ; ! needs \
QPARSER: parser QPARSER: raw raw body ; ! needs \

QPARSER: in IN: token ;
QPARSER: char CHAR: raw ;
! QPARSER: use USE: token ;

! QPARSER: unuse UNUSE: token ;
! ! QPARSER: from FROM: token "=>" expect ";" raw-until ;
! ! QPARSER: exclude EXCLUDE: token "=>" expect ";" raw-until ;
! ! QPARSER: rename RENAME: raw raw "=>" expect raw ;
! QPARSER: qualified QUALIFIED: token ;
! QPARSER: qualified-with QUALIFIED-WITH: token token ;
! QPARSER: forget FORGET: token ;


QPARSER: builtin BUILTIN: existing-class body ;


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
QPARSER: functor FUNCTOR: token parse ";FUNCTOR" parse-until ;
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
