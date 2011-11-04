! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors iterators kernel sequences ;
IN: iterators.output

MIXIN: output-iterator

GENERIC: iterator-push-back1 ( elt obj -- )

GENERIC: <output-iterator> ( length/f iterator/obj -- iterator )
