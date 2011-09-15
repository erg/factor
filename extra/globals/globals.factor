! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: assocs kernel values ;
IN: globals

VALUE: globals

ERROR: undefined-variable variable ;

\ globals [ H{ } clone ] initialize-value

: goff ( variable -- )
    [ f ] dip globals set-at ;

: gon ( variable -- )
    [ t ] dip globals set-at ;

: gget ( variable -- object )
    globals ?at [ undefined-variable ] unless ;

: gset ( object variable -- )
    globals set-at ;

: gunset ( variable -- )
    globals delete-at ;

:: gchange ( variable quot -- )
    variable globals ?at [
        quot call
    ] [
        drop
        f quot call
    ] if variable globals set-at ; inline
