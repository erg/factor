! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: arrays growable kernel sbufs sequences strings vectors ;
IN: iterators

MIXIN: iterator
GENERIC: <iterator> ( obj -- iterator )
M: iterator <iterator> ;

GENERIC: iterator>object ( iterator -- obj )

GENERIC: new-object ( n exemplar -- obj )
M: array new-object drop 0 <array> ;
M: vector new-object drop <vector> ;
M: iota new-object drop 0 <array> ;
M: string new-object drop 0 <string> ;
M: sbuf new-object drop <sbuf> ;

GENERIC: object-capacity ( obj -- n )
M: sequence object-capacity length ;
M: growable object-capacity capacity ;

