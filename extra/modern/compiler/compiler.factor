! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs classes compiler.units
constructors hash-sets hashtables io kernel make math
modern.paths modern.quick-parser modern.syntax multiline
namespaces prettyprint sequences sequences.deep sets sorting
strings words.private fry combinators quotations words.symbol ;
QUALIFIED-WITH: modern.syntax modern
FROM: syntax => f inline ;
QUALIFIED: words
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

TUPLE: qvocab namespace words classes ;
CONSTRUCTOR: <qvocab> qvocab ( -- obj )
    100 <hashtable> >>namespace
    100 <hashtable> >>words
    100 <hashtable> >>classes ;

! dict is all vocabs
TUPLE: linear-state { using hash-set } in compilation-unit? private? last-word decorators dict ;
CONSTRUCTOR: <linear-state> linear-state ( -- obj )
    HS{ } clone >>using
    V{ } clone >>decorators
    10 <hashtable> >>dict ;

: with-linear-state ( quot -- )
    [ <linear-state> \ linear-state ] dip with-variable ; inline

: current-dict ( -- qvocab )
    linear-state get dict>> ;

: current-qvocab ( -- qvocab )
    linear-state get [ dict>> ] [ in>> ] bi of ;

: make-qvocab ( name -- )
    [ <qvocab> ] dip linear-state get dict>> set-at ;

: lookup-qvocab ( name -- qvocab )
    [ linear-state get dict>> ] dip of ;

: add-using ( in -- ) linear-state get using>> adjoin ;
: set-in ( in -- ) linear-state get in<< ;
: get-in ( -- obj ) linear-state get in>> ;
: get-dict ( -- obj ) linear-state get dict>> ;
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

GENERIC: meta-pass' ( obj -- )
M: object meta-pass'
    dup name-of [
        transfer-state
        set-last-word
    ] [ drop ] if ;

M: modern:using meta-pass' object>> first [ >string add-using ] each ;
M: modern:use meta-pass' object>> first >string add-using ;
M: modern:in meta-pass' object>> first >string [ set-in ] [ make-qvocab ] bi ;
M: modern:private-begin meta-pass' drop t set-private ;
M: modern:private-end meta-pass' drop f set-private ;

M: modern:final meta-pass' add-decorator ;
M: modern:foldable meta-pass' add-decorator ;
M: modern:flushable meta-pass' add-decorator ;
M: modern:inline meta-pass' add-decorator ;
M: modern:recursive meta-pass' add-decorator ;
M: modern:deprecated meta-pass' add-decorator ;
M: modern:delimiter meta-pass' add-decorator ;

: meta-pass ( parsed -- )
    [ meta-pass' ] each transfer-state ;


ERROR: class-already-exists name vocaab ;
ERROR: no-vocab name vocab ;
: check-class ( name vocab -- name vocab )
    dup [ no-vocab ] unless
    2dup lookup-qvocab classes>> key? [
        class-already-exists
    ] when ;

ERROR: word-already-exists name vocab ;
: check-word ( name vocab -- name vocab )
    dup [ no-vocab ] unless
    2dup lookup-qvocab words>> key? [
        word-already-exists
    ] when ;

! XXX: module repository
: word-hashcode ( name vocab -- hashcode )
    [ hashcode ] bi@ hash-combine >fixnum ;

: make-word ( name vocab -- word )
    2dup word-hashcode (word) ; ! XXX: add to compilation-unit

: add-class-prop ( word -- word' )
    dup t "class" words:set-word-prop ;

: record-word ( word -- )
    dup name>> current-qvocab words>> set-at ;

: record-class ( word -- )
    dup name>> current-qvocab classes>> set-at ;

: record-namespace ( word -- )
    dup name>> current-qvocab namespace>> set-at ;

ERROR: identifier-not-found name ;
: lookup-in-linear-state ( name -- word )
    current-qvocab namespace>> ?at [ identifier-not-found ] unless ;

: make-class ( name vocab -- class )
    make-word add-class-prop ;

: create-word ( string vocab -- word )
    check-class check-word
    make-word [ record-word ] [ record-namespace ] [ ] tri ;


: create-class ( string vocab -- class )
    check-class check-word
    make-class [ record-class ] [ record-namespace ] [ ] tri ;

: create-class-word ( string vocab -- class )
    check-class check-word
    make-class { [ record-word ] [ record-class ] [ record-namespace ] [ ] } cleave ;

GENERIC: create-pass' ( obj -- )

M: object create-pass' drop ;
M: modern:singleton create-pass' [ object>> first >string ] [ in>> ] bi create-class drop ;
M: modern:singletons create-pass' [ object>> first ] [ in>> ] bi '[ >string _ create-class drop ] each ;
M: modern:symbol create-pass' [ object>> first >string ] [ in>> ] bi create-word drop ;
M: modern:symbols create-pass' [ object>> first ] [ in>> ] bi '[ >string _ create-word drop ] each ;

: create-pass ( parsed -- )
    [ create-pass' ] each ;


GENERIC: define-pass' ( obj -- )
M: modern:in define-pass' drop ;
M: modern:symbol define-pass' name-first lookup-in-linear-state define-symbol ;

: define-pass ( parsed -- )
    [ define-pass' ] each ;

: quick-compile ( seq -- linear-state )
    [
        [
            [ meta-pass ]
            [ create-pass ]
            [ define-pass ] tri
            current-qvocab namespace>> values
            [ "compiling words:" print . ]
            [ compile ] bi
            linear-state get
        ] with-linear-state
    ] with-compilation-unit ;

: quick-compile-vocab ( name -- linear-state )
    qparse-vocab quick-compile ;

: quick-compile-string ( name -- linear-state )
    qparse quick-compile ;

/*
clear
basis-source-files
[
    dup .
    quick-parse-path
    dup meta drop sift [ dup qsequence? [ in>> ] [ drop f ] if ] reject describe drop
] each


word def effect define-inline

recompile - dup def>> { } map>assoc
! modify-code-heap
*/
