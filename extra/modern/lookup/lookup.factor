! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators.short-circuit fry
hashtables io io.files kernel macros modern.parser
modern.parser.factor nested-comments prettyprint sequences
sequences.deep sets splitting strings vocabs vocabs.files
vocabs.hierarchy vocabs.loader vocabs.metadata ;
IN: modern.lookup


! Exceptions so far: SLOT: foo  foo>> >>foo
! TUPLE: bar a ;   bar?
! primitives
! "io.encodings.utf16n"  unloaded stuff?
! "system" vocab
! "syntax"
! alien: SINGLETONS: stdcall thiscall fastcall cdecl mingw ; SYMBOLS:
(*
clear basis-untracked-words
[
    first2
    [
        [ string? not ] filter
    ] [
        [ string? not ] filter
    ] bi*
    2array
] { } assoc-map-as
[ second second empty? not ] filter

clear core-vocabs
[
  [ [ vocab-words ] [ ".private" append vocab-words ] bi append [ name>> ] map ]
  [ lookup-vocab values sift concat ] bi diff natural-sort .
] each

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


: name-and-predicate ( name -- seq )
    dup "?" append 2array ;


GENERIC: object>identifiers ( object -- string )

M: sequence object>identifiers
    [ object>identifiers ] { } map-as sift ;

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
M: mstring object>identifiers drop f ;
M: mtoken object>identifiers drop f ;
M: mnumber object>identifiers drop f ;
M: escaped object>identifiers drop f ;
M: mhashtable object>identifiers drop f ;
M: marray object>identifiers drop f ;
M: mvector object>identifiers drop f ;
M: char object>identifiers drop f ;
M: tuple-literal-assoc object>identifiers drop f ;
M: tuple-literal-boa object>identifiers drop f ;


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
M: symbols object>identifiers names>> object>identifiers ;
M: slot object>identifiers name>> name>> ;
M: math object>identifiers name>> name>> ;
M: hook object>identifiers name>> name>> ;

M: singleton object>identifiers name>> name>> name-and-predicate ;

M: singletons object>identifiers
    names>> [ name>> name-and-predicate ] map concat ;

M: constructor object>identifiers name>> name>> ;
M: main object>identifiers name>> name>> ;

M: specialized-array object>identifiers class>> name>> ;
M: specialized-arrays object>identifiers classes>> ;

M: mmacro object>identifiers name>> name>> ;
M: syntax object>identifiers name>> ;
M: c-function object>identifiers name>> name>> ;
M: c-function-alias object>identifiers aliased-name>> name>> ;
M: x-function object>identifiers name>> name>> ;
M: gl-function object>identifiers name>> name>> ;

! alien/ffi
M: c-type object>identifiers name>> name>> ;
M: struct object>identifiers name>> name>> ;
M: packed-struct object>identifiers name>> name>> ;
M: library object>identifiers name>> name>> ;
M: typedef object>identifiers new>> name>> ;
M: menum object>identifiers name>> name>> ;
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
M: functor-syntax object>identifiers name>> name>> ;

! libraries
M: ebnf object>identifiers name>> name>> ;
M: mirc object>identifiers name>> name>> ;



M: protocol object>identifiers name>> name>> ;
M: article object>identifiers name>> string>> ;
M: about object>identifiers name>> string>> ;
M: single-bind object>identifiers target>> ;

M: long-string object>identifiers name>> name>> ;
M: c-global object>identifiers name>> name>> ;
M: parser object>identifiers name>> name>> ;


! nested
M: private object>identifiers body>> object>identifiers ;
M: compilation-unit object>identifiers code>> object>identifiers ;

! stack checker
M: hints object>identifiers name>> name>> ;
M: mexecute( object>identifiers drop f ;
M: mcall( object>identifiers drop f ;

! Compiler
M: codegen object>identifiers name1>> name>> ;
M: conditional object>identifiers name1>> name>> ;
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

: lookup-vocab ( vocab -- seq )
    modern-source-path dup . flush
    parse-modern-file second
    [ dup object>identifiers ] { } map>assoc ;

: each-vocab-object ( vocab -- seq )
    ;

: lookup-vocab-failures ( vocab -- seq )
    lookup-vocab [ nip not ] assoc-filter ;

: vocabs-from ( root -- vocabs )
    "" disk-vocabs-in-root/prefix
    [ don't-load? not ] filter no-prefixes
    [ name>> ] map ;

: filter-vocabs ( seq -- seq )
    [ lookup-vocab-failures ] map harvest ;

: core-vocabs ( -- seq ) "resource:core" vocabs-from ;
: basis-vocabs ( -- seq ) "resource:basis" vocabs-from ;
: extra-vocabs ( -- seq ) "resource:extra" vocabs-from ;

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

: lookup-vocab' ( vocab -- seq )
    modern-source-path dup . flush
    parse-modern-file second
    [ [ object>identifiers ] keep ] { } map>assoc
    [ drop ] assoc-filter >hashtable ;


: untracked-words ( vocab -- seq )
    [ lookup-vocab' keys ]
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
