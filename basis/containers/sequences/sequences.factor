! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors iterators iterators.input iterators.output
kernel math sequences vectors ;
IN: containers.sequences

TUPLE: sequence-iterator sequence n ;
INSTANCE: sequence-iterator input-iterator
INSTANCE: sequence-iterator output-iterator

: <sequence-iterator> ( sequence -- iterator )
    sequence-iterator new
        swap >>sequence
        0 >>n ; inline

! If we don't know the output length, use this
TUPLE: sequence-output-iterator < sequence-iterator exemplar ;

: <sequence-output-iterator> ( exemplar -- iterator )
    sequence-output-iterator new
        swap >>exemplar
        100 <vector> >>sequence
        0 >>n ; inline

M: sequence <iterator> <sequence-iterator> ;

M: sequence <output-iterator> 
    over [
        new-object <sequence-iterator>
    ] [
        nip <sequence-output-iterator>
    ] if ;

M: sequence-iterator <output-iterator>
    sequence>> <output-iterator> ;

: capacity-check? ( n obj -- ? )
    dupd object-capacity < [ 0 >= ] [ drop f ] if ; inline

M: sequence-iterator iterator-peek-front1
    [ ] [ n>> ] [ sequence>> ] tri
    2dup capacity-check? [
        nth t
    ] [
        2drop f f
    ] if ;

M: sequence-iterator iterator-advance
    [ 1 + ] change-n ;

M: sequence-iterator iterator-read-front1
    iterator-peek-front1
    [ iterator-advance ] 2dip ;

M: sequence-iterator iterator-push-back1
    [ ] [ n>> ] [ sequence>> ] tri
    2dup capacity-check? [
        [ [ 1 + ] change-n drop ] 2dip
        set-nth
    ] [
        3drop drop
    ] if ;

M: sequence-iterator iterator>object
    [ sequence>> ] [ n>> ] bi head ;

M: sequence-output-iterator iterator>object
    [ [ sequence>> ] [ n>> ] bi head ]
    [ exemplar>> ] bi like ;

M: sequence-iterator object-capacity sequence>> object-capacity ;

