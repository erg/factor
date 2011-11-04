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

M: dlist-iterator iterator-peek-front1
    [ ] [ front>> [ obj>> t ] [ f f ] if* ] bi ;

M: dlist-iterator iterator-advance
    dup [ [ next>> ] change-front ] when ;

M: dlist-iterator iterator-read-front1
    iterator-peek-front1 [ iterator-advance ] 2dip ;

M: dlist-iterator iterator-push-back1
    dlist>> push-back ;

M: dlist new-object 2drop <dlist> ;

M: dlist-iterator iterator>object dlist>> ;

M: dlist <output-iterator>
    2drop <dlist> <dlist-iterator> ; inline

M: dlist object-capacity drop f ;
