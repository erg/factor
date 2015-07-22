! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: io io.streams.document io.streams.duplex kernel
io.streams.string tools.test ;
IN: io.streams.document.tests

{
    T{ doc { object 97 }
        { start T{ pos f 0 0 } }
        { finish T{ pos f 0 1 } }
    }
} [
    "asdf" <string-reader> <document-stream> [
        read1
    ] with-input-stream
] unit-test


{
    T{ doc
        { object 115 }
        { start T{ pos { column 1 } } }
        { finish T{ pos { column 2 } } }
    }
} [
    "asdf\nfdsa" <string-reader> <document-stream> [
        read1 drop
        read1
    ] with-input-stream
] unit-test

{
    T{ doc
        { object "asdf" }
        { start T{ pos { column 0 } } }
        { finish T{ pos { column 4 } } }
    }
} [
    "asdf\nfdsa" <string-reader> <document-stream> [
        readln
    ] with-input-stream
] unit-test

{
    T{ doc
        { object "fdsa" }
        { start T{ pos { line 1 } { column 0 } } }
        { finish T{ pos { line 1 } { column 4 } } }
    }
} [
    "asdf\nfdsa" <string-reader> <document-stream> [
        readln drop
        readln
    ] with-input-stream
] unit-test

{
    T{ doc
        { object f }
        { start T{ pos { line 2 } { column 0 } } }
        { finish T{ pos { line 2 } { column 0 } } }
    }
} [
    "asdf\nfdsa" <string-reader> <document-stream> [
        readln drop
        readln drop
        readln
    ] with-input-stream
] unit-test

{
    T{ doc
        { object "fdsa\n1234\n5678\n" }
        { start T{ pos { line 1 } { column 0 } } }
        { finish T{ pos { line 1 } { column 0 } } }
    }
} [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        readln drop
        contents
    ] with-input-stream
] unit-test


{
    T{ doc
        { object "\nfds" }
        { start T{ pos { line 0 } { column 4 } } }
        { finish T{ pos { line 1 } { column 3 } } }
    }
} [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        4 read drop
        4 read
    ] with-input-stream
] unit-test

{
    T{ doc
        { object "a" }
        { start T{ pos { line 0 } { column 0 } } }
        { finish T{ pos { line 0 } { column 1 } } }
    }
    T{ doc
        { object CHAR: s }
        { start T{ pos { column 1 } } }
        { finish T{ pos { column 2 } } }
    }
} [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until
    ] with-input-stream
] unit-test


{
    T{ doc
        { object "d" }
        { start T{ pos { line 0 } { column 2 } } }
        { finish T{ pos { line 0 } { column 3 } } }
    }
    T{ doc
        { object CHAR: f }
        { start T{ pos { line 0 } { column 3 } } }
        { finish T{ pos { line 0 } { column 4 } } }
    }
} [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until 2drop
        "f" read-until
    ] with-input-stream
] unit-test

{
    T{ doc
        { object "df" }
        { start T{ pos { line 0 } { column 2 } } }
        { finish T{ pos { line 0 } { column 4 } } }
    }
    T{ doc
        { object CHAR: \n }
        { start T{ pos { column 4 } } }
        { finish T{ pos { line 1 } { column 0 } } }
    }
} [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until 2drop
        "\n" read-until
    ] with-input-stream
] unit-test

{
    T{ doc
        { object "fdsa\n" }
        { start T{ pos { line 1 } { column 0 } } }
        { finish T{ pos { line 2 } { column 0 } } }
    }
    T{ doc
        { object CHAR: 1 }
        { start T{ pos { line 2 } } }
        { finish T{ pos { line 2 } { column 1 } } }
    }
} [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until 2drop
        "\n" read-until 2drop
        "1" read-until
    ] with-input-stream
] unit-test

{ f f } [
"" [ input>document-stream "j" read-until ] with-string-reader
] unit-test

{
    T{ doc
        { object "asdf" }
        { finish T{ pos { column 4 } } }
    }
    f
} [
    "asdf" [ input>document-stream "j" read-until ] with-string-reader
] unit-test

{ 0 } [ "\n" count-trailing ] unit-test
{ 1 } [ "\na" count-trailing ] unit-test
{ 2 } [ "\nab" count-trailing ] unit-test
{ 0 } [ "\nab\n" count-trailing ] unit-test
{ 1 } [ "\nab\na" count-trailing ] unit-test
{ 2 } [ "\nab\nab" count-trailing ] unit-test

{ 0 } [ "" count-newlines ] unit-test
{ 1 } [ "\n" count-newlines ] unit-test
{ 2 } [ "\n\n" count-newlines ] unit-test
