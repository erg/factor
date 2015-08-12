! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs classes io kernel make
modern.syntax modern.paths modern.quick-parser prettyprint
sequences sequences.deep sets sorting ;
QUALIFIED-WITH: modern.syntax modern
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

GENERIC: precompile ( obj -- seq )

! GENERIC: object>quotation ( obj -- quot )

! M: sequence object>quotation [ object>quotation ] map concat call( -- ) ;

! M: function object>quotation ;
