! Copyright (C) 2009 Slava Pestov.
! See http://factorcode.org/license.txt for BSD license.
USING: functors sequences sequences.private growable
kernel words classes math parser ;
IN: vectors.functor

FUNCTOR: define-vector ( V A <A> -- )

ALIAS: <V> <${V}>
ALIAS: >V  >${V}

TUPLE: V { underlying A } { length array-capacity } ;

: <V> ( capacity -- vector ) <A> 0 V boa ; inline

M: V like
    drop dup V instance? [
        dup A instance? [ dup length V boa ] [ >V ] if
    ] unless ; inline

M: V new-sequence drop [ <A> ] [ >fixnum ] bi V boa ; inline

M: A new-resizable drop <V> ; inline

M: V new-resizable drop <V> ; inline

M: V equal? over V instance? [ sequence= ] [ 2drop f ] if ;

: >V ( seq -- vector ) V new clone-like ; inline

INSTANCE: V growable

;FUNCTOR
