! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: io io.streams.document io.streams.duplex kernel
io.streams.string tools.test ;
IN: io.streams.document.tests

[ T{ document-object { object 97 } { position T{ document-position f 0 0 } } } ]
[
    "asdf" <string-reader> <document-stream> [
        read1
    ] with-input-stream
] unit-test


[
    T{ document-object
        { object 115 }
        { position T{ document-position { column 1 } } }
    }
] [
    "asdf\nfdsa" <string-reader> <document-stream> [
        read1 drop
        read1
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "asdf" }
        { position T{ document-position { column 0 } } }
    }
] [
    "asdf\nfdsa" <string-reader> <document-stream> [
        readln
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "fdsa" }
        { position T{ document-position { line 1 } { column 0 } } }
    }
] [
    "asdf\nfdsa" <string-reader> <document-stream> [
        readln drop
        readln
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object f }
        { position T{ document-position { line 2 } { column 0 } } }
    }
] [
    "asdf\nfdsa" <string-reader> <document-stream> [
        readln drop
        readln drop
        readln
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "fdsa\n1234\n5678\n" }
        { position T{ document-position { line 1 } { column 0 } } }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        readln drop
        contents
    ] with-input-stream
] unit-test


[
    T{ document-object
        { object "\nfds" }
        { position T{ document-position { line 0 } { column 4 } } }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        4 read drop
        4 read
    ] with-input-stream
] unit-test
