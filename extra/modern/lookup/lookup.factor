! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators.short-circuit
combinators.smart fry hashtables io io.files kernel macros
modern.parser modern.parser.factor multiline namespaces
nested-comments parser prettyprint sequences sequences.deep sets
sorting splitting strings vocabs vocabs.files vocabs.hierarchy
vocabs.loader vocabs.metadata ;
IN: modern.lookup


(*
SPECIALIZED-ARRAY: uint
SLOT: foo
"syntax", "accessors"

"resource:core/sequences/sequences.factor"
{ "array-capacity" "array-capacity?" }

"resource:core/alien/alien.factor"
{ "c-ptr" "c-ptr?" }

"resource:core/layouts/layouts.factor"
{ "(first-bignum)" "(fixnum-bits)" "cell" }

"resource:core/hashtables/hashtables.factor"
{ "((empty))" "((tombstone))" "tombstone" "tombstone?" }

"resource:core/kernel/kernel.factor"
{
    "build"
    "compose"
    "compose?"
    "curry"
    "curry?"
    "null"
    "object"
}
*)

! functors
! GENERATED NAMES generated names

: constructor-name ( name -- name' ) "<" ">" surround ;
: append-main ( name -- name' ) "-main" append ;
: predicate-name ( name -- name' ) "?" append ;
: setter-name ( name -- name' ) "set-" prepend ;
: fast-name ( name -- name' ) "-fast" append ;
: attributes-name ( name -- name' ) "-attributes" append ;

: name-and-global-setter ( name -- string ) [ ] [ setter-name ] bi 2array ;
: name-and-tr-fast ( name -- pair ) [ ] [ fast-name ] bi 2array ;
: name-and-predicate ( name -- seq ) [ ] [ predicate-name ] bi 2array ;
: name-and-attributes ( name -- seq ) [ ] [ attributes-name ] bi 2array ;

: name-and-predicate-and-constructor ( name -- seq )
    [ ] [ predicate-name ] [ constructor-name ] tri 3array ;

: enum>symbols ( enum -- obj )
    {
        [ name>> name>> ]
        [ name>> name>> constructor-name ]
        [
            slots>> [ dup ptoken? [ name>> ] [ elements>> first name>> ] if ] map
            dup [ predicate-name ] map
        ]
    } { } cleave>sequence flatten ;


GENERIC: object>identifiers ( object -- string )

M: sequence object>identifiers
    [ object>identifiers ] { } map-as sift ;

HEREDOC: LOOOOOOOL

! Any
! M: object object>identifiers drop f ;

! namespace
M: comment object>identifiers drop f ;
M: using object>identifiers drop f ;
M: use object>identifiers drop f ;
M: min object>identifiers drop f ;
M: from object>identifiers drop f ;
M: exclude object>identifiers drop f ;
M: qualified object>identifiers drop f ;
M: qualified-with object>identifiers drop f ;
M: rename object>identifiers drop f ;
M: forget object>identifiers drop f ;
M: mnested-comment object>identifiers drop f ;

! word props
M: mflushable object>identifiers drop f ;
M: mfoldable object>identifiers drop f ;
M: minline object>identifiers drop f ;
M: mfinal object>identifiers drop f ;
M: mrecursive object>identifiers drop f ;

! Addons
M: mmethod object>identifiers drop f ;
M: instance object>identifiers drop f ;

! literals
M: block object>identifiers drop f ;
M: parsed-string object>identifiers drop f ;
M: ptoken object>identifiers drop f ;
M: pnumber object>identifiers drop f ;
M: escaped object>identifiers drop f ;
M: mhashtable object>identifiers drop f ;
M: marray object>identifiers drop f ;
M: mvector object>identifiers drop f ;
M: char object>identifiers drop f ;
M: tuple-literal-assoc object>identifiers drop f ;
M: tuple-literal-boa object>identifiers drop f ;

M: signature object>identifiers drop f ;

M: mprimitive object>identifiers name>> name>> ;
M: mgeneric object>identifiers name>> name>> ;
M: mgeneric# object>identifiers name>> name>> ;
M: constant object>identifiers name>> name>> ;
M: function object>identifiers name>> name>> ;
M: defer object>identifiers name>> name>> ;
M: mtuple object>identifiers name>> name>> name-and-predicate ;
M: mbuiltin object>identifiers name>> name>> name-and-predicate ;
M: merror object>identifiers name>> name>> name-and-predicate ;
M: munion object>identifiers name>> name>> name-and-predicate ;
M: mixin object>identifiers name>> name>> name-and-predicate ;
M: predicate object>identifiers name>> name>> name-and-predicate ;
M: symbol object>identifiers name>> name>> ;
M: symbols object>identifiers names>> ;
M: math object>identifiers name>> name>> ;
M: hook object>identifiers name>> name>> ;

! Generated symbols and vocabs
! XXX: accessors
M: slot object>identifiers drop f ;
M: specialized-array object>identifiers drop f ; ! class>> name>> ;
M: specialized-arrays object>identifiers drop f ; ! classes>> object>identifiers ;


M: singleton object>identifiers name>> name>> name-and-predicate ;

M: singletons object>identifiers
    names>> [ name>> name-and-predicate ] map concat ;

! C:
M: constructor object>identifiers name>> name>> ;
! CONSTRUCTOR:
M: new-constructor object>identifiers name>> name>> ;

! XXX: put a main word slot on vocab object
M: main object>identifiers drop f ; ! name>> name>> ; doesn't define, just reuses symbol


M: mmacro object>identifiers name>> name>> ;
M: syntax object>identifiers name>> ;
M: c-function object>identifiers name>> name>> ;
M: c-callback object>identifiers name>> name>> ;
M: c-function-alias object>identifiers aliased-name>> name>> ;
M: x-function object>identifiers name>> name>> ;
M: gl-function object>identifiers name>> name>> ;
M: glsl-shader object>identifiers name>> name>> ;
M: glsl-program object>identifiers name>> name>> ;
M: uniform-tuple object>identifiers name>> name>> ;
M: subroutine object>identifiers name>> name>> ;

! alien/ffi
M: c-type object>identifiers name>> name>> ;
M: struct object>identifiers name>> name>> name-and-predicate ;
M: packed-struct object>identifiers name>> name>> ;
M: union-struct object>identifiers name>> name>> ;
M: library object>identifiers drop f ;
M: typedef object>identifiers new>> name>> ;
M: menum object>identifiers enum>symbols ;
M: mpointer object>identifiers drop f ;

! Locals/fry/etc
M: locals-typed object>identifiers name>> name>> ;
M: locals-function object>identifiers name>> name>> ;
M: locals-memo object>identifiers name>> name>> ;
M: locals-macro object>identifiers name>> name>> ;
M: locals-mmethod object>identifiers drop f ;
M: locals-block object>identifiers drop f ;
M: memo object>identifiers name>> name>> ;
M: typed object>identifiers name>> name>> ;
M: mfry object>identifiers drop f ;


! Language features
M: alias object>identifiers name>> name>> ;
M: functor object>identifiers name>> name>> ;

! Weird functor syntax defines functor-specific syntax stuff
! not actual symbols
M: functor-syntax object>identifiers drop f ; ! name>> name>> ;

! libraries
M: ebnf object>identifiers name>> name>> ;
M: mirc object>identifiers name>> name>> ;
M: 8-bit object>identifiers name>> name>> name-and-predicate ;


M: protocol object>identifiers name>> name>> ;
M: article object>identifiers drop f ; ! name>> string>> ;
M: about object>identifiers drop f ; ! name>> string>> ;
M: single-bind object>identifiers target>> ;

M: long-string object>identifiers name>> name>> ;
M: c-global object>identifiers name>> name>> name-and-global-setter ;
M: mparser object>identifiers name>> name>> ;


! nested
M: private object>identifiers body>> object>identifiers ;
M: compilation-unit object>identifiers code>> object>identifiers ;

! stack checker
M: hints object>identifiers drop f ; ! name>> name>> ;
M: mexecute( object>identifiers drop f ;
M: mcall( object>identifiers drop f ;

! Compiler
M: codegen object>identifiers drop f ; ! name1>> name>> ; ! only defines a method
M: conditional object>identifiers drop f ; ! name1>> name>> ; ! only defines a method

M: mfoldable-insn object>identifiers name>> name>> ;
M: mflushable-insn object>identifiers name>> name>> ;
M: minsn object>identifiers name>> name>> ;
M: mvreg-insn object>identifiers name>> name>> ;
M: mregisters object>identifiers names>> object>identifiers ;
M: mhi-registers object>identifiers names>> object>identifiers ;

! simd
M: simd-128 object>identifiers name>> name>> ;
M: simd-128-cord object>identifiers drop f ;
M: simd-intrinsic object>identifiers name>> name>> ;
M: locals-simd-intrinsic object>identifiers name>> name>> ;

M: mdata-map( object>identifiers drop f ;
M: mdata-map!( object>identifiers drop f ;

! cocoa/objc
M: import object>identifiers name>> name>> ;

! More:
M: mtest object>identifiers name>> name>> ;
M: mreset object>identifiers drop f ;
M: mspecial-object object>identifiers name>> name>> ;
M: tr object>identifiers name>> name>> name-and-tr-fast ;
M: mintersection object>identifiers name>> name>> name-and-predicate ;
M: method-literal object>identifiers drop f ;
M: unicode-category object>identifiers name>> name>> name-and-predicate ;
M: unicode-category-not object>identifiers name>> name>> name-and-predicate ;

M: main-window object>identifiers name>> name>> ;
M: solution object>identifiers name>> name>> append-main ;
M: game object>identifiers name>> name>> name-and-attributes ;

LOOOOOOOL
drop


MACRO: any-predicate? ( words -- quot )
    [ '[ _ execute ] ] map
    [ [ ] ] [ '[ _ 1|| ] ] if-empty ;

: modern-if-available ( path -- path' )
    dup ".factor" ?tail [
        ".modern" append
        dup exists? [
            nip
        ] [
            drop
        ] if
    ] [
        drop
    ] if ;

ERROR: not-a-source-path path ;
: force-modern-path ( path -- path' )
    ".factor" ?tail [ ".modern" append ] [ not-a-source-path ] if ;
: modern-docs-path ( path -- path' )
    vocab-docs-path modern-if-available ;
: modern-tests-path ( path -- path' )
    vocab-tests-path modern-if-available ;
: modern-source-path ( path -- path' )
    vocab-source-path modern-if-available ;
: modern-syntax-path ( path -- path' )
    vocab-source-path ".factor" ?tail drop "-syntax.modern" append ;

: force-modern-docs-path ( path -- path' )
    vocab-docs-path force-modern-path ;
: force-modern-tests-path ( path -- path' )
    vocab-tests-path force-modern-path ;
: force-modern-source-path ( path -- path' )
    vocab-source-path force-modern-path ;

: parse-vocab>assoc ( vocab -- seq )
    modern-source-path
    parse-modern-file
    [ dup object>identifiers ] { } map>assoc ;

: vocab>namespace ( vocab -- public private )
    parse-vocab>assoc
    [ first private? not ] partition
    [ values flatten ] bi@ ;

: lookup-vocab-failures ( vocab -- seq )
    parse-vocab>assoc [ nip not ] assoc-filter ;

: vocabs-from ( root -- vocabs )
    "" disk-vocabs-in-root/prefix
    [ don't-load? not ] filter no-prefixes
    [ name>> ] map ;

: filter-vocabs ( seq -- seq )
    [ lookup-vocab-failures ] map harvest ;

: core-vocabs ( -- seq ) "resource:core" vocabs-from ;
: basis-vocabs ( -- seq ) "resource:basis" vocabs-from ;
: extra-vocabs ( -- seq ) "resource:extra" vocabs-from ;
: all-vocabs ( -- seq )
    [
        core-vocabs
        basis-vocabs
        extra-vocabs
    ] { } append-outputs-as ;

: filter-exists ( seq -- seq' ) [ exists? ] filter ;

: all-syntax-paths ( -- seq )
    all-vocabs [ modern-syntax-path ] map filter-exists ;

: all-source-paths ( -- seq )
    all-vocabs [ modern-source-path ] map filter-exists ;

: all-docs-paths ( -- seq )
    all-vocabs [ modern-docs-path ] map filter-exists ;

: all-tests-paths ( -- seq )
    all-vocabs [ modern-tests-path ] map filter-exists ;

: all-factor-files ( -- seq )
    [ all-syntax-paths all-source-paths all-docs-paths all-tests-paths ] { } append-outputs-as ;

: diff-bad-basis-vocabs ( seq -- seq' )
    { } diff ;

: diff-bad-extra-vocabs ( seq -- seq' )
    { "modern.parser.terse" "yaml.conversion" } diff ;

: load-core ( -- seq )
    core-vocabs filter-vocabs ;

: load-basis ( -- seq )
    "resource:basis" vocabs-from
    diff-bad-basis-vocabs
    filter-vocabs ;

: load-extra ( -- seq )
    "resource:extra" vocabs-from
    diff-bad-extra-vocabs
    filter-vocabs ;

: parse-vocab>assoc' ( vocab -- seq )
    modern-source-path dup . flush
    parse-modern-file
    [ [ object>identifiers ] keep ] { } map>assoc
    [ drop ] assoc-filter >hashtable ;


: untracked-words ( vocab -- seq )
    [ parse-vocab>assoc' keys ]
    [
        [ vocab-words ] [ ".private" append vocab-words ] bi append
        [ name>> ] map [ flatten ] bi@ [ diff ] [ swap diff ] 2bi
    ] bi 2array ;

: vocabs-untracked-words ( seq -- seq' )
    [ dup untracked-words ] { } map>assoc ;

: core-untracked-words ( -- seq )
    core-vocabs vocabs-untracked-words ;

: basis-untracked-words ( -- seq )
    basis-vocabs diff-bad-basis-vocabs vocabs-untracked-words ;

: extra-untracked-words ( -- seq )
    extra-vocabs diff-bad-extra-vocabs vocabs-untracked-words ;

: vocab-names>syntax ( strings -- seq )
    [ modern-syntax-path ] map [ exists? ] filter ;

: core-syntax-files ( -- seq ) core-vocabs vocab-names>syntax ;
: basis-syntax-files ( -- seq ) basis-vocabs vocab-names>syntax ;
: extra-syntax-files ( -- seq ) extra-vocabs vocab-names>syntax ;

: load-core-syntax ( -- seq ) core-syntax-files [ parse-modern-file ] map ;
: load-basis-syntax ( -- seq ) basis-syntax-files [ parse-modern-file ] map ;
: load-extra-syntax ( -- seq ) extra-syntax-files [ parse-modern-file ] map ;

: load-namespace ( name -- triple )
    dup
    parse-vocab>assoc [ first private? not ] partition
    [ values flatten ] bi@ 3array ;

: rewrite-modern ( path -- )
    [ parse-modern-file ] keep write-modern-file ;

: ?rewrite-modern ( path -- )
    dup exists? [ rewrite-modern ] [ drop ] if ;

: rewrite-source ( vocab-names -- )
    [ modern-source-path ?rewrite-modern ] each ;

: rewrite-docs ( vocab-names -- )
    [ modern-docs-path ?rewrite-modern ] each ;

: rewrite-tests ( vocab-names -- )
    [ modern-tests-path ?rewrite-modern ] each ;

: namespace-ok? ( triple -- ? )
    first3 [ empty? ] both? nip ;

! Words that we parse but are not in the loaded image
: check-namespace ( triple -- triple )
    [ first ]
    [
        first2 [ vocab-words [ name>> ] map ] dip swap diff natural-sort
    ] [
        first3 nip
        [ ".private" append vocab-words [ name>> ] map ] dip
        swap diff natural-sort
    ] tri 3array ;

! Words in the image that don't have a symbol on disk
: check-namespace2 ( triple -- triple )
    [ first ]
    [ first2 [ vocab-words [ name>> ] map ] dip diff natural-sort ]
    [ first3 nip [ ".private" append vocab-words [ name>> ] map ] dip diff natural-sort ] tri 3array ;

: vocab-loaded? ( name -- ? )
    dictionary get at ;

: check-loaded-namespaces ( names -- triples )
    [ vocab-loaded? ] filter
    [ load-namespace ] map
    [ check-namespace ] map
    [ first3 [ empty? ] both? nip ] reject ;

: check-loaded-namespaces2 ( names -- triples )
    [ vocab-loaded? ] filter
    [ load-namespace ] map
    [ check-namespace2 ] map
    [ first3 [ empty? ] both? nip ] reject ;

: test-namespace ( name -- ? )
    load-namespace check-namespace namespace-ok? ;

: test-namespace2 ( name -- ? )
    load-namespace check-namespace2 namespace-ok? ;

: failing-namespaces ( names -- names' )
    [ dup test-namespace ] { } map>assoc
    [ second ] reject keys ;

: failing-namespaces2 ( names -- names' )
    [ dup test-namespace2 ] { } map>assoc
    [ second ] reject keys ;
