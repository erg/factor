! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors iterators kernel ;
IN: iterators.input

MIXIN: input-iterator
INSTANCE: input-iterator iterator
GENERIC: iterator-read-front1 ( iterator -- iterator' elt ? )
GENERIC: iterator-peek-front1 ( iterator -- iterator' elt ? )
GENERIC: iterator-advance ( iterator -- iterator' )

TUPLE: peeked-input-iterator peeked iterator ;

M: peeked-input-iterator iterator-peek-front1
    [ ] [ peeked>> ] bi t ;

M: peeked-input-iterator iterator-read-front1
    [ iterator>> ] [ peeked>> ] bi t ;

M: peeked-input-iterator iterator-advance
    iterator>> ;
