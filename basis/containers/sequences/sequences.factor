! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors iterators iterators.input iterators.output
kernel math sequences ;
IN: containers.sequences

TUPLE: sequence-iterator sequence n ;
INSTANCE: sequence-iterator input-iterator
INSTANCE: sequence-iterator output-iterator

: <sequence-iterator> ( sequence -- iterator )
    sequence-iterator new
        swap >>sequence
        0 >>n ; inline

M: sequence <iterator> <sequence-iterator> ;

M: sequence-iterator iterator-read-front1
    [ ] [ n>> ] [ sequence>> ] tri
    2dup bounds-check? [
        [ [ 1 + ] change-n drop ] 2dip
        nth t
    ] [
        3drop f f
    ] if ;

M: sequence-iterator iterator-push-back1
    [ ] [ n>> ] [ sequence>> ] tri
    2dup bounds-check? [
        [ [ 1 + ] change-n drop ] 2dip
        set-nth t
    ] [
        3drop drop f
    ] if ;
