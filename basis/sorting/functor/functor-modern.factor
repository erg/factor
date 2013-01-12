! Copyright (C) 2009 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: functors kernel math.order sequences sorting ;
IN: sorting.functor

FUNCTOR: define-sorting ( NAME QUOT -- )

ALIAS: NAME<=> ${NAME}<=>
ALIAS: NAME>=< ${NAME}>=<

: NAME<=> ( obj1 obj2 -- <=> ) QUOT compare ;
: NAME>=< ( obj1 obj2 -- >=< ) NAME<=> invert-comparison ;

;FUNCTOR
