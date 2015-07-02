! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: io io.streams.document io.streams.duplex kernel
io.streams.string tools.test ;
IN: io.streams.document.tests

{
    T{ document-object { object 97 }
        { start T{ document-position f 0 0 } }
        { finish T{ document-position f 0 1 } }
    }
} [
    "asdf" <string-reader> <document-stream> [
        read1
    ] with-input-stream
] unit-test


[
    T{ document-object
        { object 115 }
        { start T{ document-position { column 1 } } }
        { finish T{ document-position { column 2 } } }
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
        { start T{ document-position { column 0 } } }
        { finish T{ document-position { line 1 } { column 0 } } }
    }
] [
    "asdf\nfdsa" <string-reader> <document-stream> [
        readln
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "fdsa" }
        { start T{ document-position { line 1 } { column 0 } } }
        { finish T{ document-position { line 2 } { column 0 } } }
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
        { start T{ document-position { line 2 } { column 0 } } }
        { finish T{ document-position { line 3 } { column 0 } } }
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
        { start T{ document-position { line 1 } { column 0 } } }
        { finish T{ document-position { line 1 } { column 0 } } }
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
        { start T{ document-position { line 0 } { column 4 } } }
        { finish T{ document-position { line 1 } { column 3 } } }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        4 read drop
        4 read
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "a" }
        { start T{ document-position { line 0 } { column 0 } } }
        { finish T{ document-position { line 0 } { column 1 } } }
    }
    T{ document-object
        { object CHAR: s }
        { start T{ document-position { column 1 } } }
        { finish T{ document-position { column 2 } } }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until
    ] with-input-stream
] unit-test


[
    T{ document-object
        { object "d" }
        { start T{ document-position { line 0 } { column 2 } } }
        { finish T{ document-position { line 0 } { column 3 } } }
    }
    T{ document-object
        { object CHAR: f }
        { start T{ document-position { line 0 } { column 3 } } }
        { finish T{ document-position { line 0 } { column 4 } } }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until 2drop
        "f" read-until
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "df" }
        { start T{ document-position { line 0 } { column 2 } } }
        { finish T{ document-position { line 0 } { column 4 } } }
    }
    T{ document-object
        { object CHAR: \n }
        { start T{ document-position { column 4 } } }
        { finish T{ document-position { line 1 } { column 0 } } }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until 2drop
        "\n" read-until
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "fdsa\n" }
        { start T{ document-position { line 1 } { column 0 } } }
        { finish T{ document-position { line 2 } { column 0 } } }
    }
    T{ document-object
        { object CHAR: 1 }
        { start T{ document-position { line 2 } } }
        { finish T{ document-position { line 2 } { column 1 } } }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until 2drop
        "\n" read-until 2drop
        "1" read-until
    ] with-input-stream
] unit-test

[ f f ] [
"" [ input>document-stream "j" read-until ] with-string-reader
] unit-test

{
    T{ document-object
        { object "asdf" }
        { finish T{ document-position { column 4 } } }
    }
    f
} [
    "asdf" [ input>document-stream "j" read-until ] with-string-reader
] unit-test
