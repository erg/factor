! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators.short-circuit fry
hashtables io io.files kernel macros modern.parser
modern.parser.factor nested-comments prettyprint sequences
sequences.deep sets splitting strings vocabs vocabs.hierarchy
vocabs.loader vocabs.metadata ;
FROM: modern.parser.factor => union in? macro locals-mmethod?
enum nested-comment? ;
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


GENERIC: object>identifiers* ( object -- string )

: object>identifiers ( object -- seq )
    object>identifiers* dup string? [ 1array ] when ;

M: object object>identifiers*
    drop f ;

M: mprimitive object>identifiers*
    name>> name>> ;
    
M: mgeneric object>identifiers*
    name>> name>> ;

M: mgeneric# object>identifiers*
    name>> name>> ;

M: constant object>identifiers*
    name>> name>> ;
    
M: function object>identifiers*
    name>> name>> ;
    
M: defer object>identifiers*
    name>> name>> ;
    
M: mtuple object>identifiers*
    name>> name>> name-and-predicate ;

M: mbuiltin object>identifiers*
    name>> name>> name-and-predicate ;

M: merror object>identifiers*
    name>> name>> name-and-predicate ;
    
M: union object>identifiers*
    name>> name>> name-and-predicate ;
    
M: mixin object>identifiers*
    name>> name>> name-and-predicate ;
    
M: predicate object>identifiers*
    name>> name>> name-and-predicate ;
    
M: symbol object>identifiers*
    name>> name>> ;
    
M: symbols object>identifiers*
    names>> [ name>> ] map ;

M: slot object>identifiers*
    name>> name>> ;

M: math object>identifiers*
    name>> name>> ;

M: hook object>identifiers*
    name>> name>> ;


M: singleton object>identifiers*
    name>> name>> name-and-predicate ;

M: singletons object>identifiers*
    names>> [ name>> name-and-predicate ] map concat ;

M: constructor object>identifiers*
    name>> name>> ;

M: main object>identifiers*
    name>> name>> ;

M: locals-function object>identifiers*
    name>> name>> ;
M: locals-memo object>identifiers*
    name>> name>> ;
M: locals-macro object>identifiers*
    name>> name>> ;
M: memo object>identifiers*
    name>> name>> ;
M: typed object>identifiers*
    name>> name>> ;
M: specialized-array object>identifiers*
    class>> name>> ;
M: specialized-arrays object>identifiers*
    classes>> ;
    
M: macro object>identifiers*
    name>> name>> ;    
! XXX: todo
M: syntax object>identifiers*
    name>> ;
M: c-function object>identifiers*
    name>> name>> ;
M: c-function-alias object>identifiers*
    aliased-name>> name>> ;
M: x-function object>identifiers*
    name>> name>> ;
M: gl-function object>identifiers*
    name>> name>> ;    
    
M: enum object>identifiers*
    name>> name>> ;
    

! M: ebnf object>identifiers*
  !  name>> name>> ;

  
  
M: library object>identifiers*
    name>> name>> ;
M: struct object>identifiers*
    name>> name>> ;
M: packed-struct object>identifiers*
    name>> name>> ;
M: typedef object>identifiers*
    new>> name>> ;
M: locals-typed object>identifiers*
    name>> name>> ;
    
    
M: alias object>identifiers*
    name>> name>> ;
M: hints object>identifiers*
    name>> name>> ;  
M: functor object>identifiers*
    name>> name>> ;
M: functor-syntax object>identifiers*
    name>> name>> ;

M: ebnf object>identifiers*
    name>> name>> ;

    
M: c-type object>identifiers*
    name>> name>> ;    
M: protocol object>identifiers*
    name>> name>> ; 
M: article object>identifiers*
    name>> string>> ;     
M: about object>identifiers*
    name>> string>> ;
M: single-bind object>identifiers*
    target>> ;


M: long-string object>identifiers*
    name>> name>> ;  

M: c-global object>identifiers*
    name>> name>> ;      

M: parser object>identifiers*
    name>> name>> ;


M: literal-parser object>identifiers*
    name>> name>> ;

    
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

: modern-source-path ( path -- path' )
    vocab-source-path modern-if-available ;
    
: lookup-vocab ( vocab -- seq )
    modern-source-path dup . flush
    parse-modern-file second
    [ dup object>identifiers ] { } map>assoc ;

: lookup-vocab-failures ( vocab -- seq )
    lookup-vocab [ nip not ] assoc-filter ;
    
: filter-failures ( seq -- seq' )
    [
        drop {
            ! Comments
            comment? nested-comment?
            
            ! Lookup
            from? qualified? qualified-with? in? using? use? rename? exclude? forget?
            
            ! Word properties
            flushable? inline? recursive? foldable? final?
            
            ! Enclosers
            compilation-unit? private?
            
            ! Object literals
            block? locals-block? fry?
            mstring? mtoken? mnumber?
            marray? mhashtable? mvector? char? pointer? escaped?
            tuple-literal-assoc? tuple-literal-boa?
            
            ! Function instances
            instance? mmethod? locals-mmethod?
            
            ! Calls
            execute(? call(? data-map(? data-map!(?

            ! Basis/compiler
            import?
            codegen?
            conditional?
            foldable-insn? flushable-insn?
            insn? vreg-insn?
            registers? hi-registers?
            simd-128? simd-128-cord? simd-intrinsic?
            locals-simd-intrinsic?

        } any-predicate? not
    ] assoc-filter ;

: vocabs-from ( root -- vocabs )
    "" disk-vocabs-in-root/prefix
    [ don't-load? not ] filter no-prefixes
    [ name>> ] map ;
    
: filter-vocabs ( seq -- seq )
    [ lookup-vocab-failures filter-failures ] map harvest ;

: core-vocabs ( -- seq ) "resource:core" vocabs-from ;
: basis-vocabs ( -- seq ) "resource:basis" vocabs-from ;
: extra-vocabs ( -- seq ) "resource:extra" vocabs-from ;

: diff-bad-basis-vocabs ( seq -- seq' )
    { "specialized-vectors" "tools.scaffold" } diff ;

: diff-bad-extra-vocabs ( seq -- seq' )
    { "irc.messages" "yaml.conversion" } diff ;
    
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
