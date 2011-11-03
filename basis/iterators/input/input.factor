! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: ;
IN: iterators.input

MIXIN: input-iterator
GENERIC: iterator-read-front1 ( obj -- elt ? )
GENERIC: iterator-advance ( obj -- )
GENERIC: iterator-peek-front1 ( obj -- elt ? )
