! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors constructors destructors fry io kernel math
namespaces sequences ;
IN: io.streams.position

TUPLE: position-stream stream { n integer } ;

CONSTRUCTOR: position-stream ( stream -- position-stream ) ;

: with-advance-1 ( stream quot -- seq )
    [ call ] 2keep drop
    [ 1 + ] change-n drop ; inline

: with-advance ( stream quot -- seq )
    [ call ] 2keep drop over
    '[ _ dup sequence? [ length ] when + ] change-n drop ; inline

: with-advance' ( stream quot -- seq )
    [ call ] 2keep drop pick
    '[ _ dup sequence? [ length ] when + ] change-n drop ; inline

M: position-stream stream-readln
    [ stream>> stream-readln ] with-advance ;

M: position-stream stream-read1
    [ stream>> stream-read1 ] with-advance-1 ;

M: position-stream stream-read-unsafe
    [ stream>> stream-read-unsafe ] with-advance ;

M: position-stream stream-read-until
    [ stream>> stream-read-until ] with-advance' ;

M: position-stream stream-element-type
    stream>> stream-element-type ;

M: position-stream dispose*
    stream>> dispose ;


: input>position-stream ( -- )
    input-stream [ <position-stream> ] change ;

: input-position ( -- n ) input-stream get n>> ;

: output>position-stream ( -- )
    output-stream [ <position-stream> ] change ;

: output-position ( -- n ) output-stream get n>> ;
