! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors io io.encodings io.ports iterators
iterators.input iterators.output kernel ;
IN: streams

TUPLE: stream-iterator stream ;

INSTANCE: stream-iterator iterator

: <stream-iterator> ( stream -- iterator )
    stream-iterator new
        swap >>stream ; inline

M: input-port <iterator>
    <stream-iterator> ;
    
M: decoder <iterator>
    <stream-iterator> ;

M: decoder <output-iterator>
    drop V{ } <output-iterator> ;
    
M: stream-iterator iterator-peek-front1
    [ ] [ stream>> stream-read1 dup >boolean ] bi ;
    
M: stream-iterator iterator-advance ;

M: decoder object-capacity drop f ;