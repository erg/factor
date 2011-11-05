! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays combinators combinators.short-circuit
iterators iterators.input iterators.output kernel locals
math.order ;
IN: iterators.zip

TUPLE: zip-iterator input0 input1 ;

INSTANCE: zip-iterator iterator

: new-iterator ( obj0 obj1 class -- iterator )
    [ [ <iterator> ] bi@ ] dip
    new
        swap >>input1
        swap >>input0 ; inline

TUPLE: shortest-zip-iterator < zip-iterator ;
: <shortest-zip-iterator> ( obj0 obj1 -- zip-iterator )
    shortest-zip-iterator new-iterator ; inline

TUPLE: longest-zip-iterator < zip-iterator ;
: <longest-zip-iterator> ( obj0 obj1 -- zip-iterator )
    longest-zip-iterator new-iterator ; inline

TUPLE: same-length-zip-iterator < zip-iterator ;
: <same-length-zip-iterator> ( obj0 obj1 -- zip-iterator )
    same-length-zip-iterator new-iterator ; inline

: zip-iterator-object-capacity ( zip-iterator -- x/f y/f )
    [ input0>> object-capacity ]
    [ input1>> object-capacity ] bi ;

M: same-length-zip-iterator object-capacity
    {
        [ input0>> object-capacity ]
        [ input1>> object-capacity ]
    } 1|| ;

M: longest-zip-iterator object-capacity
    zip-iterator-object-capacity [ 0 or ] bi@ max ;

M: shortest-zip-iterator object-capacity
    zip-iterator-object-capacity 2dup and [ min ] [ or ] if ;


: >zip-iterator-peek< ( zip-iterator -- iterator0 value0 0? iterator1 value1 1? )
    [ input0>> iterator-peek-front1 ]
    [ input1>> iterator-peek-front1 ] bi ; inline

ERROR: not-same-length iterator ;

M:: same-length-zip-iterator iterator-peek-front1 ( zip-iterator -- zip-iterator' value ? )
    zip-iterator >zip-iterator-peek< :> ( iterator0 value0 0? iterator1 value1 1? )
    iterator0 iterator1 <same-length-zip-iterator>
    0? 1? and [
        value0 value1 2array t
    ] [
        0? 1? or [
            not-same-length
        ] [
            f f
        ] if
    ] if ;

M:: shortest-zip-iterator iterator-peek-front1 ( zip-iterator -- zip-iterator' value ? )
    zip-iterator >zip-iterator-peek< :> ( iterator0 value0 0? iterator1 value1 1? )
    iterator0 iterator1 <same-length-zip-iterator>
    0? 1? and [
        value0 value1 2array t
    ] [
        f f
    ] if ;

M:: longest-zip-iterator iterator-peek-front1 ( zip-iterator -- zip-iterator' value ? )
    zip-iterator >zip-iterator-peek< :> ( iterator0 value0 0? iterator1 value1 1? )
    iterator0 iterator1 <longest-zip-iterator>
    0? 1? 2array {
        { { f f } [ { f f } f ] }
        { { f t } [ { f value1 } t ] }
        { { t f } [ { value0 f } t ] }
        { { t t } [ { value0 value1 } t ] }
    } case ;


: advance-zip-iterator ( zip-iterator -- iterator0 iterator1 )
    [ input0>> iterator-advance ] [ input1>> iterator-advance ] bi ; inline

M: same-length-zip-iterator iterator-advance
    advance-zip-iterator <same-length-zip-iterator> ;

M: shortest-zip-iterator iterator-advance
    advance-zip-iterator <shortest-zip-iterator> ;

M: longest-zip-iterator iterator-advance
    advance-zip-iterator <longest-zip-iterator> ;


M: zip-iterator iterator-read-front1
    [ iterator-peek-front1 [ drop ] 2dip ]
    [ iterator-advance -rot ] bi ;

M: zip-iterator <output-iterator>
    drop V{ } <output-iterator> ;
