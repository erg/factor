! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs compiler.units constructors effects fry
kernel math.parser modern.lookup modern.parser
modern.parser.factor namespaces nested-comments quotations
sequences vocabs.parser words ;
IN: modern.compiler

TUPLE: compiler in namespaces ;
CONSTRUCTOR: <compiler> compiler ( -- obj )
    H{ } clone >>namespaces ;

GENERIC: lookup-token ( obj -- obj' )

M: mstring lookup-token string>> ;
M: mnumber lookup-token n>> string>number ;

GENERIC: mcompile ( obj -- quot )

: set-in ( name -- ) compiler get in<< ;

: add-use ( name -- )
    [ lookup-vocab ] keep compiler get namespaces>> set-at ;

: add-using ( names -- )
    [ add-use ] each ;


M: min mcompile name>> set-in [ ] ;
M: use mcompile strings>> name>> add-use [ ] ;
M: using mcompile strings>> add-using [ ] ;

M: function mcompile
    [ name>> name>> '[ _ current-vocab create-word ] ]
    [ body>> [ lookup-token ] map >quotation ]
    [ signature>> [ in>> ] [ out>> ] bi <effect> ] tri
    '[ @ _ _ define-declared ]  ;

: with-compiler ( quot -- )
    [ <compiler> \ compiler ] dip
    with-variable ; inline

ERROR: in-form-missing ;
: ensure-in ( -- )
    compiler get in>> [ in-form-missing ] unless ;

: compile-modern ( seq -- )
    [
        [ mcompile ] map [ ] concat-as
        ensure-in
        compiler get in>> name>> [ create-vocab drop ] [ ] bi
        '[ _ set-current-vocab ] prepose
        '[ call( -- ) ] with-compilation-unit
    ] with-compiler ;

: compile-modern-string ( str -- )
    parse-modern-string compile-modern ;
