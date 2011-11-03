! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: arrays growable kernel sequences vectors ;
IN: iterators

GENERIC: <iterator> ( obj -- iterator )
GENERIC: <output-iterator> ( length/f obj -- iterator )
GENERIC: iterator>object ( iterator -- obj )

GENERIC: new-object ( n exemplar -- obj )
M: array new-object drop 0 <array> ;
M: vector new-object drop <vector> ;
M: iota new-object drop 0 <array> ;

GENERIC: object-capacity ( obj -- n )
M: sequence object-capacity length ;
M: growable object-capacity capacity ;

