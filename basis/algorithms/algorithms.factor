! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: iterators iterators.input iterators.output kernel locals
fry math generalizations ;
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
: predicate-yes ( -- quot' ) [ drop t ] ; inline
: predicate-no ( -- quot' ) [ drop f ] ; inline

:: iterator-find ( iterator0 quot: ( ..a obj -- ..b ? ) -- iterator obj )
    iterator0 iterator-peek-front1 :> ( iterator1 obj present? )
    present? [
        obj quot call [
            iterator1 obj
        ] [
            iterator0 iterator-advance quot iterator-find
        ] if
    ] [
        iterator1 f
    ] if ; inline recursive

: find ( obj quot -- obj iterator )
    [ <iterator> ] dip [ iterator-find ] [ drop ] 2bi ; inline

: each ( obj quot -- obj )
    [ <iterator> ] dip never iterator-find 2drop ; inline

:: copy-pred ( input0 output quot pred -- input output )
    input0 iterator-peek-front1 :> ( input1 elt1 present? )
    present? [
        elt1 pred call [
            elt1 quot call output iterator-push-back1
            input1 iterator-advance output quot pred copy-pred
        ] [
            input1 output
        ] if
    ] [
        input1 output
    ] if ; inline

: make-copy-iterators ( obj exemplar -- input-iterator output-iterator )
    [ drop <iterator> ]
    [ [ object-capacity ] dip <output-iterator> ] 2bi ;

: (map-as) ( obj exemplar quot pred -- obj' input-iterator output-iterator )
    [ make-copy-iterators ] 2dip copy-pred ; inline

: map-as ( obj quot exemplar -- obj' )
    swap predicate-yes (map-as) nip iterator>object ; inline

: map ( obj quot -- obj' ) over map-as ; inline

! : map! ( obj quot -- obj' ) over map-as! ; inline

TUPLE: lazy-map quot ;
C: <lazy-map> lazy-map

TUPLE: lazy-filter quot ;
C: <lazy-filter> lazy-filter

: <map> ( quot -- obj ) <lazy-map> ; inline

: <filter> ( quot -- obj ) <lazy-filter> ; inline

: <map-as> ( obj/quot quot exemplar -- obj/quot lazy-map exemplar ) [ <map> ] dip ; inline

: take-as ( obj pred exemplar -- iterator obj )
    swap [ [ ] ] dip (map-as) iterator>object ; inline

: take ( obj pred -- iterator obj' ) over take-as ; inline

: map-sum ( ... seq quot: ( ... elt -- ... n ) -- ... n )
    [ 0 ] 2dip [ dip + ] curry [ swap ] prepose each ; inline

: count ( ... seq quot: ( ... elt -- ... ? ) -- ... n )
    [ 1 0 ? ] compose map-sum ; inline
