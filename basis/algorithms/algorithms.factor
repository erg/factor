! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: iterators iterators.input kernel locals ;
IN: algorithms

: never ( quot -- quot' ) [ f ] compose ; inline
: always ( quot -- quot' ) [ t ] compose ; inline

:: iterator-find ( iterator quot: ( ..a obj -- ..b ? ) -- obj )
    iterator iterator-read-front1 :> ( obj present? ) present? [
        obj quot call [
            obj
        ] [
            iterator quot iterator-find
        ] if
    ] [
        f
    ] if ; inline recursive

: find ( obj quot -- obj iterator )
    [ <iterator> ] dip [ iterator-find ] [ drop ] 2bi ; inline

: each ( obj quot -- obj iterator )
    [ <iterator> ] dip never iterator-find drop ; inline
