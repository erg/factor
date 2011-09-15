! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: assocs kernel values ;
IN: globals

VALUE: globals

ERROR: undefined-global global ;

\ globals [ H{ } clone ] initialize-value

: goff ( variable -- )
    [ f ] dip \ globals get-value set-at ;

: gon ( variable -- )
    [ t ] dip \ globals get-value set-at ;

: gget ( variable -- object )
    \ globals get-value ?at [ undefined-global ] unless ;

: gset ( object variable -- )
    \ globals get-value set-at ;

: gunset ( variable -- )
    \ globals get-value delete-at ;

