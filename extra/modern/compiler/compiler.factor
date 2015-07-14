! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs classes.mixin classes.predicate
combinators compiler.units constructors effects fry generic
generic.parser kernel literals.private math math.parser
modern.lookup modern.parser modern.parser.factor namespaces
nested-comments parser prettyprint quotations sequences
sequences.extras sets splitting strings vocabs vocabs.parser
words ;
IN: modern.compiler

GENERIC: lookup-token ( obj -- obj' )

M: parsed-string lookup-token string>> ;
M: parsed-number lookup-token n>> string>number ;
ERROR: word-not-found word ;
M: parsed-token lookup-token name>> search ;
M: marray lookup-token elements>> [ lookup-token ] map expand-literals ;
M: escaped lookup-token name>> <parsed-token> lookup-token <wrapper> ;

M: block lookup-token
    body>> [ lookup-token ] map >quotation ;

: >modern ( name -- name' ) ;

: >modern2 ( name -- name' )
    "-modern" ?tail drop "-modern" append ;

: add-in-vocab ( name -- )
    >modern
    [ create-vocab drop ]
    [ set-current-vocab ] bi ;

GENERIC: precompile-word ( string -- )

M: string precompile-word
    [ current-vocab create-word ] keep current-vocab words>> set-at ;


GENERIC: precompile ( obj -- )
M: comment precompile drop ;
M: defer precompile name>> name>> precompile-word ;
M: mgeneric precompile name>> name>> precompile-word ;
M: mbuiltin precompile
    name>> name>>
    [ precompile-word ]
    [ "?" append precompile-word ] bi ;

M: mprimitive precompile name>> name>> precompile-word ;
M: use precompile drop ;
M: using precompile drop ;
M: min precompile name>> name>> >modern [ add-in-vocab ] [ set-current-vocab ] bi ;
M: private precompile
    begin-private
    body>> [
        object>identifiers dup string? [
            precompile-word
        ] [
            [ precompile-word ] each
        ] if
    ] each
    end-private ;

M: munion precompile name>> name>> precompile-word ;

M: merror precompile
    name>> name>>
    [ precompile-word ]
    [ "?" append precompile-word ] bi ;
M: function precompile name>> name>> precompile-word ;
M: mmethod precompile drop ;
M: minline precompile drop ;
M: mrecursive precompile drop ;
M: instance precompile drop ;
M: predicate precompile
    name>> name>>
    [ precompile-word ]
    [ "?" append precompile-word ] bi ;

M: mtuple precompile
    name>> name>>
    [ precompile-word ]
    [ "?" append precompile-word ] bi ;

GENERIC: mcompile ( obj -- quot )
M: comment mcompile drop [ ] ;
M: mbuiltin mcompile drop [ ] ;
M: mprimitive mcompile drop [ ] ;


M: min mcompile
    name>> name>> >modern [ add-in-vocab ] keep '[ _ set-current-vocab ] ;
M: use mcompile strings>> name>> use-vocab [ ] ;
M: using mcompile strings>> [ use-vocab ] each [ ] ;

M: function mcompile
    ! [ name>> name>> '[ _ create-word-in dup set-last-word ] ]
    [ name>> name>> '[ _ current-vocab create-word dup set-last-word ] ] ! create-word-in
    [ body>> [ lookup-token ] map >quotation ]
    [ signature>> [ in>> ] [ out>> ] bi <effect> ] tri
    '[ @ _ _ define-declared ]  ;

! M: mgeneric mcompile '[ @ _ _ define-generic ] ;

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

M: defer mcompile drop [ ] ;

M: private mcompile
    begin-private
    body>> [ mcompile ] map [ ] concat-as
    end-private ;

! M: mtuple mcompile [

: compile-modern ( seq -- )
    ! "syntax" use-vocab
    [
        [ [ precompile ] each ]
        [ [ mcompile ] map [ ] concat-as ] bi
        call( -- )
    ] with-compilation-unit ;

: compile-modern-string ( str -- )
    parse-modern-string compile-modern ;

: compile-vocab ( name -- )
    modern-source-path parse-modern-file compile-modern ;
