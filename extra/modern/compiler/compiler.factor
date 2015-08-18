! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.

USING: accessors arrays assocs classes constructors hash-sets io
kernel make modern.paths modern.quick-parser modern.syntax
namespaces prettyprint sequences sequences.deep sets sorting
strings multiline hashtables ;
QUALIFIED-WITH: modern.syntax modern
FROM: syntax => f inline ;
IN: modern.compiler

: path>parsers ( name -- seq )
    quick-parse-path
    [ [ class-of , ] deep-each ] { } make members ;

: paths>parsers ( names -- seq )
    [ path>parsers ] map concat members ;


: path>lexers ( name -- seq )
    quick-parse-path
    [ [ dup class-of array = [ , ] [ drop ] if ] deep-each ] { } make rest members ;

: paths>lexers ( names -- seq )
    [ path>lexers ] map concat [ first ] map >out members ;


: paths>top-level-forms ( paths -- seq )
    [ dup quick-parse-path [ slice? ] filter ] { } map>assoc harvest-values ;
    ! values concat members natural-sort ;

: paths>top-level-forms. ( paths -- )
    paths>top-level-forms [ first2 [ print ] [ >out members natural-sort . ] bi* ] each ;

: core-parsers ( -- seq ) core-source-files paths>parsers ;
: core-lexers ( -- seq ) core-source-files paths>lexers ;
: basis-parsers ( -- seq ) basis-source-files paths>parsers core-parsers diff ;
: basis-lexers ( -- seq ) basis-source-files paths>lexers ;
: extra-parsers ( -- seq ) extra-source-files paths>parsers core-parsers diff basis-parsers diff ;
: extra-lexers ( -- seq ) extra-source-files paths>lexers ;

: print-parsers ( seq -- )
    members natural-sort
    [ name>> "M: modern:" " precompile ;" surround print ] each ;

TUPLE: linear-state { using hash-set } in compilation-unit? private? last-word decorators namespace ;
CONSTRUCTOR: <linear-state> linear-state ( -- obj )
    HS{ } clone >>using
    V{ } clone >>decorators ;

: with-linear-state ( quot -- )
    [ <linear-state> \ linear-state ] dip with-variable ; inline

: add-using ( in -- ) linear-state get using>> adjoin ;
: set-in ( in -- ) linear-state get in<< ;
: get-in ( -- obj ) linear-state get in>> ;
: set-private ( ? -- ) linear-state get private?<< ;
: get-private ( -- obj ) linear-state get private?>> ;
: set-compilation-unit ( ? -- ) linear-state get compilation-unit?<< ;
: get-compilation-unit ( -- obj ) linear-state get compilation-unit?>> ;
: set-last-word ( name -- ) linear-state get last-word<< ;
: get-last-word ( -- obj ) linear-state get last-word>> ;
: add-decorator ( obj -- ) linear-state get decorators>> push ;
: transfer-decorators ( -- )
    linear-state get [
        get-last-word [ decorators<< ] [ drop ] if*
        V{ } clone
    ] change-decorators drop ;

: set-word-in ( -- )
    get-in [ get-last-word [ in<< ] [ drop ] if* ] when* ;
: set-word-private ( -- )
    get-private [ get-last-word [ private?<< ] [ drop ] if* ] when* ;
: set-word-compilation-unit ( -- )
    get-compilation-unit [ get-last-word [ compilation-unit?<< ] [ drop ] if* ] when* ;

DEFER: name-of
: name-first ( object -- string ) object>> first >string ;
: name-second ( object -- string ) object>> first >string ;
: name-sequence ( object -- strings ) object>> first [ name-of ] map ;

GENERIC: name-of ( obj -- name )
M: modern:function name-of name-first ;
M: modern:constructor name-of name-first ;
M: modern:generic name-of name-first ;
M: modern:generic# name-of name-first ;
M: modern:math name-of name-first ;
M: modern:mixin name-of name-first ;
M: modern:tuple name-of name-first ;
M: modern:error name-of name-first ;
M: modern:builtin name-of name-first ;
M: modern:primitive name-of name-first ;
M: modern:union name-of name-first ;
M: modern:intersection name-of name-first ;
M: modern:predicate name-of name-first ;
M: modern:slot name-of name-first ;
M: modern:hook name-of name-first ;
M: modern:method name-of object>> first2 2array ;
M: modern:constant name-of name-first ;
M: modern:singleton name-of name-first ;
M: modern:singletons name-of name-sequence ;
M: modern:symbol name-of name-first ;
M: modern:symbols name-of name-sequence ;
M: modern:defer name-of name-first ;
M: modern:alias name-of name-first ;
M: modern:c-function name-of name-first ;
M: modern:gl-function name-of name-first ;
M: modern:macro name-of name-second ;
M: object name-of drop f ;

: transfer-state ( -- )
    set-word-compilation-unit
    set-word-private
    set-word-in transfer-decorators ;

GENERIC: meta-pass' ( obj -- obj )
M: object meta-pass'
    dup name-of [
        transfer-state
        [ set-last-word ] [ ] bi
    ] [ ] if ;

M: modern:using meta-pass' object>> first [ >string add-using ] each f ;
M: modern:use meta-pass' object>> first >string add-using f ;
M: modern:in meta-pass' object>> first >string set-in f ;
M: modern:private-begin meta-pass' drop t set-private f ;
M: modern:private-end meta-pass' drop f set-private f ;

M: modern:final meta-pass' add-decorator f ;
M: modern:foldable meta-pass' add-decorator f ;
M: modern:flushable meta-pass' add-decorator f ;
M: modern:inline meta-pass' add-decorator f ;
M: modern:recursive meta-pass' add-decorator f ;
M: modern:deprecated meta-pass' add-decorator f ;
M: modern:delimiter meta-pass' add-decorator f ;

: meta-pass ( parsed -- parsed linear-state )
    [
        [ meta-pass' ] map
        transfer-state
        \ linear-state get
    ] with-linear-state ;

TUPLE: qvocab namespace words classes ;
CONSTRUCTOR: <qvocab> qvocab ( -- obj )
    100 <hashtable> >>namespace
    100 <hashtable> >>words
    100 <hashtable> >>classes ;

: current-qvocab ( -- qvocab )
    ;

ERROR: class-already-exists name ;
: check-class ( string -- string )
    dup current-qvocab classes>> get key? [
        class-already-exists
    ] when ;

ERROR: word-already-exists name ;
: check-word ( string -- string )
    dup current-qvocab words>> get key? [
        word-already-exists
    ] when ;

: (create-word) ( string -- word ) ;
: (create-class) ( string -- class ) ;

: create-word ( string -- word )
    check-class check-word (create-word) ;

: create-class ( string -- class )
    check-class check-word (create-class) ;

: create-class-word ( string -- class )
    check-word check-class (create-word) (create-class) ;

GENERIC: create-pass' ( obj -- obj/seq )

M: object create-pass' drop f ;
M: modern:singleton create-pass' object>> first >string create-class ;
M: modern:singletons create-pass' object>> first [ >string create-class ] map ;


: create ( parsed linear-state -- parsed )
    [
        [ create-pass' dup sequence? [ 1array ] unless ] map concat
        \ linear-state get
    ] with-linear-state ;

: define-pass ( parsed linear-state -- parsed )
    [
        \ linear-state get
    ] with-linear-state ;

: quick-compile ( seq -- )
    meta-pass create-pass define-pass ;

/*
clear
basis-source-files
[
    dup .
    quick-parse-path
    dup meta drop sift [ dup qsequence? [ in>> ] [ drop f ] if ] reject describe drop
] each
*/
