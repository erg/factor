! Copyright (C) 2009 Slava Pestov.
! See http://factorcode.org/license.txt for BSD license.
USING: functors destructors accessors kernel parser words
effects generalizations sequences ;
IN: alien.destructors

TUPLE: alien-destructor alien ;

FUNCTOR: define-destructor ( F -- )

TUPLE: ${F}-destructor < alien-destructor ;

: <${F}-destructor> ( alien -- destructor )
    ${F}-destructor boa ; inline

: N ( -- n ) ${F} stack-effect out>> length ;

M: ${F}-destructor dispose alien>> N ndrop ;

: &${F} ( alien -- alien ) dup <${F}-destructor> &dispose drop ; inline

: |${F} ( alien -- alien ) dup <${F}-destructor> |dispose drop ; inline

;FUNCTOR

SYNTAX: DESTRUCTOR: scan-word define-destructor ;
