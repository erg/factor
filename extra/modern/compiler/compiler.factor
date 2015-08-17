! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs classes constructors hash-sets io
kernel make modern.paths modern.quick-parser modern.syntax
namespaces prettyprint sequences sequences.deep sets sorting
strings ;
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
    [ linear-state new \ linear-state ] dip with-variable ; inline

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

: name-first ( object -- string )
    object>> first >string ;
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
M: modern:singletons name-of object>> first [ >string ] map ;
M: modern:symbol name-of name-first ;
M: modern:symbols name-of object>> first [ >string ] map ;
M: modern:defer name-of name-first ;
! M: modern:defer name-of name-first ; ! XXX: defer with decorators?
M: object name-of drop f ;

: transfer-state ( -- )
    set-word-compilation-unit
    set-word-private
    set-word-in transfer-decorators ;

GENERIC: meta' ( obj -- obj )
M: object meta'
    dup name-of [
        transfer-state
        [ set-last-word ] [ ] bi
    ] [ ] if ;

M: modern:using meta' object>> first [ >string add-using ] each f ;
M: modern:use meta' object>> first >string add-using f ;
M: modern:in meta' object>> first >string set-in f ;
M: modern:private-begin meta' drop t set-private f ;
M: modern:private-end meta' drop f set-private f ;

M: modern:final meta' add-decorator f ;
M: modern:foldable meta' add-decorator f ;
M: modern:flushable meta' add-decorator f ;
M: modern:inline meta' add-decorator f ;
M: modern:recursive meta' add-decorator f ;
M: modern:deprecated meta' add-decorator f ;
M: modern:delimiter meta' add-decorator f ;

: meta ( parsed -- parsed linear-state )
    [
        [ meta' ] map
        transfer-state
        \ linear-state get
    ] with-linear-state ;
