! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors f.lexer kernel math sequences ;
QUALIFIED-WITH: io.streams.document io
IN: f.recalculate

ERROR: line-bounds-error n obj ;

: check-bounds ( n obj -- n obj )
    over 0 >= [ line-bounds-error ] unless ;

GENERIC: rebase-line-offset ( offset object -- )

M: sequence rebase-line-offset
    [ rebase-line-offset ] with each ;

M: lexed rebase-line-offset
    tokens>> [ rebase-line-offset ] with each ;

M: io:token rebase-line-offset
    [ + ] change-line# drop ;
    
: calculate-offset ( n object -- offset )
    check-bounds first-token line#>> - ;

: rebase-line ( n object -- )
    [ calculate-offset ] keep rebase-line-offset ;
