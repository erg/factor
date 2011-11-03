! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: iterators iterators.input iterators.output kernel locals
fry math ;
FROM: sequences => collector-for ;
IN: algorithms

! if you have lazy map and copy (from input iterator to output iterator) to start with
! then you can build strict map and map-as from those
! separate out the input-to-output iterator copy from making the output sequence + iterator
! that would also let you implement map! easily
! just use the same iterator for input and output
! instead of lazy map you could have map-copy
! which does read-front (quotation) write-front
! input output copy would be [ input read-front ] [ output write-front ] while
! input output quot map-copy would be [ input read-front ] [ quot call output write-front ] while
! then "seq quot map" would be "seq >input-iterator seq new-output-iterator-like quot map-copy"
! the output iterator could be a vector/dlist writer that pushes onto the end, or a mutable range over a preallocated fixed-size sequence
! you can choose that given an exemplar type and a length i think
! so it only needs to be generic on the exemplar type

: never ( quot -- quot' ) [ f ] compose ; inline
: always ( quot -- quot' ) [ t ] compose ; inline
: invert ( quot -- quot' ) [ not ] compose ; inline

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

:: map-copy ( input output quot -- output )
    [ input iterator-read-front1 ]
    [ quot call output iterator-push-back1 ] while drop
    output iterator>object ; inline

: (map-as) ( obj exemplar quot -- obj' input-iterator )
    [
        [ drop <iterator> ]
        [ [ object-capacity ] dip <output-iterator> ] 2bi
    ] dip [ map-copy ] 3keep 2drop ; inline

: map-as ( obj quot exemplar -- obj' )
    swap (map-as) drop ; inline

: map ( obj quot -- obj' ) over map-as ; inline

! : map! ( obj quot -- obj' ) over map-as! ; inline

TUPLE: lazy-map quot ;
C: <lazy-map> lazy-map

TUPLE: lazy-filter quot ;
C: <lazy-filter> lazy-filter

: <map> ( quot -- obj ) <lazy-map> ; inline
: <filter> ( quot -- obj ) <lazy-filter> ; inline

: <map-as> ( obj/quot quot exemplar -- obj/quot lazy-map exemplar ) [ <map> ] dip ; inline

! : take-as ( obj quot exemplar -- obj' iterator ) [ '[ over @ ] ] dip (map-as) ; inline

! : take ( obj quot -- obj' iterator ) over take-as ; inline

: map-sum ( ... seq quot: ( ... elt -- ... n ) -- ... n )
    [ 0 ] 2dip [ dip + ] curry [ swap ] prepose each ; inline

: count ( ... seq quot: ( ... elt -- ... ? ) -- ... n )
    [ 1 0 ? ] compose map-sum ; inline
