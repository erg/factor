! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors dlists iterators iterators.input
iterators.output kernel deques ;
IN: containers.dlists

TUPLE: dlist-iterator dlist front back ;

: <dlist-iterator> ( dlist -- iterator )
    dlist-iterator new
        swap [ >>dlist ] keep
        [ front>> >>front ] keep
        back>> >>back ; inline

M: dlist <iterator> <dlist-iterator> ;

M: dlist-iterator iterator-read-front1
    dup front>> [
        [ [ next>> ] change-front drop ] dip
        obj>> t
    ] [
        drop f f
    ] if* ;

M: dlist-iterator iterator-push-back1
    dlist>> push-back ;

M: dlist new-object 2drop <dlist> ;

M: dlist iterator-as>output-iterator ( iterator exemplar -- iterator' )
    2drop <dlist> <iterator> ;

M: dlist-iterator iterator>object dlist>> ;

M: dlist <output-iterator>
    2drop <dlist> <dlist-iterator> ; inline

M: dlist object-capacity drop f ;
