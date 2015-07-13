! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs classes.mixin classes.predicate
combinators compiler.units constructors effects fry generic
generic.parser kernel math math.parser modern.lookup
modern.parser modern.parser.factor namespaces nested-comments
parser prettyprint quotations sequences sequences.extras sets
splitting vocabs vocabs.parser words literals.private ;
IN: modern.compiler

! "arrays" modern-source-path parse-modern-file compile-modern
(*
"USE: math
IN: scratchpad2
: foo ( -- seq ) { 1 2 \ + } ;"  parse-modern-string
*)


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
M: marray lookup-token elements>> [ lookup-token ] map expand-literals ;
M: escaped lookup-token name>> <mtoken> lookup-token <wrapper> ;

M: block lookup-token
    body>> [ lookup-token ] map >quotation ;

: namespace-exists? ( name -- ? )
    [ compiler get namespaces>> ] dip '[ name>> _ = ] find nip ;

: >modern ( name -- name' )
    "-modern" ?tail drop "-modern" append ;

: add-in-vocab ( name -- )
    [
        >modern
        dup namespace-exists? [
            drop
        ] [
            create-vocab compiler get namespaces>> push
        ] if
    ] [ compiler get in<< ] bi ;

: precompile-word ( name -- )
    [ current-vocab create-word ] keep current-vocab words>> set-at ;


GENERIC: precompile ( obj -- )
M: comment precompile drop ;
M: mbuiltin precompile
    name>> name>>
    [ precompile-word ]
    [ "?" append precompile-word ] bi ;

M: mprimitive precompile name>> name>> precompile-word ;
M: use precompile drop ;
M: using precompile drop ;
M: min precompile name>> name>> >modern [ add-in-vocab ] [ set-current-vocab ] bi ;
M: function precompile name>> name>> precompile-word ;
M: mmethod precompile drop ;
M: minline precompile drop ;
M: instance precompile drop ;
M: predicate precompile
    name>> name>>
    [ precompile-word ]
    [ "?" append precompile-word ] bi ;

GENERIC: mcompile ( obj -- quot )
M: comment mcompile drop [ ] ;
M: mbuiltin mcompile drop [ ] ;
M: mprimitive mcompile drop [ ] ;

: add-use ( name -- )
    compiler get
    [ using>> adjoin ]
    [ [ lookup-vocab ] dip namespaces>> adjoin ] 2bi ;

: add-using ( names -- )
    [ add-use ] each ;

M: min mcompile
    name>> name>> >modern [ add-in-vocab ] keep '[ _ set-current-vocab ] ;
M: use mcompile strings>> name>> add-use [ ] ;
M: using mcompile strings>> add-using [ ] ;

M: function mcompile
    [ name>> name>> '[ _ current-vocab create-word dup set-last-word ] ] ! create-word-in
    [ body>> [ lookup-token ] map >quotation ]
    [ signature>> [ in>> ] [ out>> ] bi <effect> ] tri
    '[ @ _ _ define-declared ]  ;

M: mmethod mcompile
    [ [ class>> ] [ name>> ] bi [ lookup-token ] bi@ '[ _ _ create-method ] ]
    [ body>> [ lookup-token ] map >quotation ] bi
    '[ @ _ define ] drop [ ] ;

M: minline mcompile
    drop
    [ last-word make-inline ] ;

M: instance mcompile
    [ class>> ] [ mixin>> ] bi
    [ lookup-token ] bi@
    '[ _ _ add-mixin-instance ] drop [ ] ;

M: predicate mcompile
    [ [ name>> ] [ superclass>> ] bi [ lookup-token ] bi@ ]
    [ body>> [ lookup-token ] map >quotation ] bi
    '[ _ _ _ define-predicate-class ] ;

: with-compiler ( quot -- )
    [ <compiler> \ compiler ] dip
    with-variable ; inline

! ERROR: in-form-missing ;
! : ensure-in ( -- )
    ! compiler get in>> [ in-form-missing ] unless ;

: compile-modern ( seq -- )
    [
        "syntax" add-use
        [
            [ [ precompile ] each ]
            [ [ mcompile ] map [ ] concat-as ] bi
            call( -- )
        ] with-compilation-unit
        compiler get .
    ] with-compiler ;

: compile-modern-string ( str -- )
    parse-modern-string compile-modern ;
