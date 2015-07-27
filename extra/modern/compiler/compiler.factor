! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs classes.mixin classes.predicate
combinators compiler.units constructors effects fry generic
generic.parser kernel literals.private math math.parser
modern.lookup modern.parser modern.parser.factor modern.paths
multiline namespaces parser prettyprint
quotations sequences sequences.extras sets splitting strings
vocabs vocabs.parser words ;
IN: modern.compiler

GENERIC: lookup-token ( obj -- obj' )

M: pstring lookup-token string>> ;
! M: pnumber lookup-token n>> string>number ;
! ERROR: word-not-found word ;
! M: ptoken lookup-token name>> search ;
! M: parray lookup-token elements>> [ lookup-token ] map expand-literals ;
! M: pescaped lookup-token name>> ptoken new swap >>object lookup-token <wrapper> ;
! M: pblock lookup-token body>> [ lookup-token ] map >quotation ;

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
M: pcomment precompile drop ;
M: pdefer precompile name>> name>> precompile-word ;
M: pgeneric precompile name>> name>> precompile-word ;
M: pbuiltin precompile
    name>> name>>
    [ precompile-word ]
    [ "?" append precompile-word ] bi ;

M: pprimitive precompile name>> name>> precompile-word ;
M: puse precompile drop ;
M: pusing precompile drop ;
M: pin precompile name>> name>> >modern [ add-in-vocab ] [ set-current-vocab ] bi ;
M: pprivate-begin precompile
    begin-private
    body>> [
        object>identifiers dup string? [
            precompile-word
        ] [
            [ precompile-word ] each
        ] if
    ] each
    end-private ;

M: punion precompile name>> name>> precompile-word ;

M: perror precompile
    name>> name>>
    [ precompile-word ]
    [ "?" append precompile-word ] bi ;
M: pfunction precompile name>> name>> precompile-word ;
M: pmethod precompile drop ;
M: pinline precompile drop ;
M: precursive precompile drop ;
M: pinstance precompile drop ;
M: ppredicate precompile
    name>> name>>
    [ precompile-word ]
    [ "?" append precompile-word ] bi ;

M: ptuple precompile
    name>> name>>
    [ precompile-word ]
    [ "?" append precompile-word ] bi ;

GENERIC: mcompile ( obj -- quot )
/*
M: pcomment mcompile drop [ ] ;
M: pbuiltin mcompile drop [ ] ;
M: pprimitive mcompile drop [ ] ;


M: pin mcompile
    name>> name>> >modern [ add-in-vocab ] keep '[ _ set-current-vocab ] ;
M: puse mcompile object>> name>> use-vocab [ ] ;
M: pusing mcompile object>> [ use-vocab ] each [ ] ;

M: pfunction mcompile
    ! [ name>> name>> '[ _ create-word-in dup set-last-word ] ]
    [ name>> name>> '[ _ current-vocab create-word dup set-last-word ] ] ! create-word-in
    [ body>> [ lookup-token ] map >quotation ]
    [ signature>> [ in>> ] [ out>> ] bi <effect> ] tri
    '[ @ _ _ define-declared ]  ;

! M: mgeneric mcompile '[ @ _ _ define-generic ] ;

M: pmethod mcompile
    [ [ class>> ] [ name>> ] bi [ lookup-token ] bi@ '[ _ _ create-method ] ]
    [ body>> [ lookup-token ] map >quotation ] bi
    '[ @ _ define ] drop [ ] ;

M: pinline mcompile
    drop
    [ last-word make-inline ] ;

M: pipnstance mcompile
    [ class>> ] [ mixin>> ] bi
    [ lookup-token ] bi@
    '[ _ _ add-mixin-instance ] drop [ ] ;

M: ppredicate mcompile
    [ [ name>> ] [ superclass>> ] bi [ lookup-token ] bi@ ]
    [ body>> [ lookup-token ] map >quotation ] bi
    '[ _ _ _ define-predicate-class ] ;

M: defer mcompile drop [ ] ;

M: pprivate-begin mcompile
    begin-private
    body>> [ mcompile ] map [ ] concat-as
    end-private ;

! M: mtuple mcompile [
*/

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
