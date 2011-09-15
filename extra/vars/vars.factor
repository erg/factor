! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: assocs continuations fry kernel math sequences threads
values vectors ;
IN: vars

ERROR: undefined-var var ;

: vchange ( variable quot -- )
    over vars 2dup at [
        2nip
        [ [ nip ] dip last swap call ] keep set-last
    ] [
        [ f swap call 1vector ] 2dip set-at
        drop
    ] if* ; inline

: voff ( variable -- ) [ drop f ] vchange ; inline

: von ( variable -- ) [ drop t ] vchange ; inline

: vget ( variable -- object )
    vars ?at [
        last
    ] [
        undefined-var
    ] if ; inline

: vset ( object variable -- )
    swap '[ drop _ ] vchange ; inline

: vunset ( variable -- )
    vars delete-at ;

<PRIVATE

! Used to implement with-var
: push-var ( value variable -- )
    vars 2dup at [
        2nip push
    ] [
        [ 1vector ] 2dip set-at
    ] if* ; inline

: pop-var ( variable -- )
    dup vars 2dup at [
        dup length dup 1 > [
            1 - swap set-length 3drop
        ] [
            2drop delete-at drop
        ] if
    ] [
        drop undefined-var
    ] if* ; inline

PRIVATE>

: with-var ( object variable quot -- )
    [ [ push-var ] keep ] dip 
    swap '[ _ pop-var ] [ ] cleanup ; inline

