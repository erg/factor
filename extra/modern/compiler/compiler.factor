! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs compiler.units constructors effects fry
kernel math.parser modern.lookup modern.parser
modern.parser.factor namespaces nested-comments quotations
sequences vocabs vocabs.parser words ;
IN: modern.compiler

TUPLE: compiler in using namespaces ;
CONSTRUCTOR: <compiler> compiler ( -- obj )
    HS{ } clone >>using
    V{ } clone >>namespaces ;

ERROR: multiple-words-found name seq ;
ERROR: no-word-found name ;
: lookup-word ( string -- word )
    dup compiler get namespaces>>
    [ words>> at ] with [ ] map-filter sift f like {
        { [ dup length 1 > ] [ multiple-words-found ] }
        { [ dup length 1 = ] [ first ] }
        [ drop no-word-found ]
    } cond nip ;

GENERIC: lookup-token ( obj -- obj' )

M: mstring lookup-token string>> ;
M: mnumber lookup-token n>> string>number ;
ERROR: word-not-found word ;
M: mtoken lookup-token
    name>> dup lookup-word [ nip ] [ word-not-found ] if* ;

GENERIC: mcompile ( obj -- quot )

: namespace-exists? ( name -- ? )
    [ compiler get namespaces>> ] dip '[ name>> _ = ] find nip ;

: add-in-vocab ( name -- )
    [
        dup namespace-exists?
        [ drop ] [ <vocab> compiler get namespaces>> push ] if
    ] [ compiler get in<< ] bi ;

: add-use ( name -- )
    compiler get
    [ using>> adjoin ]
    [ [ lookup-vocab ] dip namespaces>> adjoin ] 2bi ;

: add-using ( names -- )
    [ add-use ] each ;

M: min mcompile name>> name>> [ add-in-vocab ] keep '[ _ set-current-vocab ] ;
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

! ERROR: in-form-missing ;
! : ensure-in ( -- )
    ! compiler get in>> [ in-form-missing ] unless ;

: compile-modern ( seq -- )
    [
        [ mcompile ] map [ ] concat-as
        '[ call( -- ) ] with-compilation-unit
        compiler get .
    ] with-compiler ;

: compile-modern-string ( str -- )
    parse-modern-string compile-modern ;
