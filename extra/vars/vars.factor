! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: assocs continuations fry kernel math sequences threads
values vectors globals ;
IN: vars

: vchange ( variable quot -- )
    over vars at [
        [ nip ] dip
        [ last swap call ] keep set-last
    ] [
        gchange
    ] if* ; inline

: voff ( variable -- ) [ drop f ] vchange ; inline

: von ( variable -- ) [ drop t ] vchange ; inline

: vget ( variable -- object )
    vars ?at [
        last
    ] [
        gget
    ] if ; inline

: vset ( object variable -- )
    swap '[ drop _ ] vchange ; inline

<PRIVATE

! Used to implement with-var; don't call directly
: push-var ( value variable -- )
    vars 2dup at [
        2nip push
    ] [
        [ 1vector ] 2dip set-at
    ] if* ; inline

ERROR: unbalanced-with-var variable ;

: pop-var ( variable -- )
    dup vars 2dup at [
        dup length dup 1 > [
            1 - swap set-length 3drop
        ] [
            2drop delete-at drop
        ] if
    ] [
        drop unbalanced-with-var
    ] if* ; inline

: vunset ( variable -- )
    vars
    2dup delete-at* nip [
        2drop
    ] [
        drop gunset
    ] if ;

PRIVATE>

: with-var ( object variable quot -- )
    [ [ push-var ] keep ] dip 
    swap '[ _ pop-var ] [ ] cleanup ; inline

