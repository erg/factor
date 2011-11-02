! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors iterators kernel sequences ;
IN: iterators.output

MIXIN: output-iterator

GENERIC: iterator-push-back1 ( elt obj -- )

: iterator>output-iterator ( iterator -- iterator' )
    sequence>> clone <iterator> ;

GENERIC: iterator-as>output-iterator ( iterator exemplar -- iterator' )

M: sequence iterator-as>output-iterator ( iterator exemplar -- iterator' )
    [ object-capacity ] dip new-object <iterator> ;
