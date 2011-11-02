! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: iterators iterators.input iterators.output kernel locals
fry math ;
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

: each ( obj quot -- obj )
    [ <iterator> ] dip never iterator-find drop ; inline

: map-as ( obj quot exemplar -- obj' )
    [ <iterator> ] 2dip
    pick swap iterator-as>output-iterator [
        '[ _ iterator-push-back1 ] compose never iterator-find drop
    ] keep iterator-like ; inline

: map ( obj quot -- obj' )
    over map-as ; inline

: map-sum ( ... seq quot: ( ... elt -- ... n ) -- ... n )
    [ 0 ] 2dip [ dip + ] curry [ swap ] prepose each ; inline

: count ( ... seq quot: ( ... elt -- ... ? ) -- ... n )
    [ 1 0 ? ] compose map-sum ; inline
