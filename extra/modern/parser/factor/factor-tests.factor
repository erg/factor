! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: ascii assocs io.encodings.utf8 io.files
io.streams.document kernel modern.lookup modern.parser
modern.parser.factor modern.paths multiline sequences sets
tools.test vocabs.hierarchy vocabs.loader ;
IN: modern.parser.factor.tests

{
    {
        T{ parray
            { object
                {
                    T{ ptext
                        { object "{" }
                        { finish T{ pos { column 1 } } }
                    }
                    {
                        T{ doc
                            { start T{ pos { column 2 } } }
                            { object "1" }
                            { finish T{ pos { column 3 } } }
                        }
                        T{ doc
                            { start T{ pos { column 4 } } }
                            { object "2" }
                            { finish T{ pos { column 5 } } }
                        }
                        T{ doc
                            { start T{ pos { column 6 } } }
                            { object "3" }
                            { finish T{ pos { column 7 } } }
                        }
                    }
                    T{ ptext
                        { start T{ pos { column 8 } } }
                        { object "}" }
                        { finish T{ pos { column 9 } } }
                    }
                }
            }
        }
    }
} [ "{ 1 2 3 }" parse-modern-string ] unit-test

{
    {
        T{ pstring
            { object
                {
                    T{ doc
                        { object 34 }
                        { finish T{ pos { column 1 } } }
                    }
                    T{ ptoken
                        { start T{ pos { column 1 } } }
                        { object "Added \\\"" }
                        { finish T{ pos { column 9 } } }
                    }
                    T{ doc
                        { start T{ pos { column 9 } } }
                        { object 34 }
                        { finish T{ pos { column 10 } } }
                    }
                }
            }
        }
    }
} [ "\"Added \\\"\"" parse-modern-string ] unit-test

{
    {
        T{ heredoc
            { object
                {
                    T{ ptext
                        { object "HEREDOC:" }
                        { finish T{ pos { column 8 } } }
                    }
                    T{ ptext
                        { start T{ pos { column 9 } } }
                        { object "lol" }
                        { finish T{ pos { column 12 } } }
                    }
                    T{ doc
                        { start T{ pos { line 1 } } }
                        { object "something\n" }
                        { finish T{ pos { line 2 } } }
                    }
                    T{ ptext
                        { start T{ pos { line 2 } } }
                        { object "lol" }
                        { finish T{ pos { line 2 } { column 3 } } }
                    }
                }
            }
        }
    }
} [
"HEREDOC: lol
something
lol" parse-modern-string
] unit-test

: check-parsed-string ( string -- ? )
    dup parse-modern-string write-modern-string sequence= ;

: check-parsed-file ( path -- ? )
    [ utf8 file-contents ]
    [ parse-modern-file write-modern-string ] bi sequence= ;

: replace-parsed-file ( path -- )
    [ parse-modern-file ] keep
    write-modern-file ;

{ t } [ "resource:core/alien/alien.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/alien/strings/strings.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/arrays/arrays.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/assocs/assocs.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/byte-arrays/byte-arrays.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/combinators/combinators.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/compiler/units/units.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/kernel/kernel.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/layouts/layouts.factor" check-parsed-file ] unit-test

{ t } [ "resource:core/lexer/lexer.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/make/make.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/math/math.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/parser/parser.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/parser/notes/notes.factor" check-parsed-file ] unit-test

{ t } [ "resource:core/sequences/sequences.factor" check-parsed-file ] unit-test


{ t } [
    "resource:core" disk-vocabs-in-root
    ! [ vocab? ] filter
    [ vocab-source-path ] map sift
    {
        "resource:core/vocabs/loader/test/a/a.factor" ! parser error
        "resource:core/vocabs/loader/test/b/b.factor" ! parser error
        "resource:core/vocabs/loader/test/c/c.factor" ! parser error
        "resource:core/vocabs/loader/test/d/d.factor" ! no eol
        "resource:core/vocabs/loader/test/l/l.factor" ! no eol
    } diff
    ! [ parse-modern-file ] map
    [ check-parsed-file ] all?
] unit-test

! Test multiline string experimental syntax
CONSTANT: tools-scaffold-string """m"[${example-indent}"Example:"
${example-indent}{ $example "USING: ${example-using} ;"
${example-indent}    ""
${example-indent}    ""
${example-indent}}]""""

! { t } [ tools-scaffold-string parse-modern-string length 1 = ] unit-test
! { t } [ tools-scaffold-string parse-modern-string ?last pstring? ] unit-test


: check-parser-exact ( string -- ? )
    [ parse-modern-string write-modern-string ] keep = ;

: check-parsed-exact ( string -- ? )
    parse-modern-string [ write-modern-string parse-modern-string ] keep = ;

{ { } } [
    all-source-paths [ dup check-parsed-file ] { } map>assoc [ nip ] assoc-reject
] unit-test

{ { } } [
    all-docs-paths [ dup check-parsed-file ] { } map>assoc [ nip ] assoc-reject
] unit-test

{ { } } [
    all-tests-paths [ dup check-parsed-file ] { } map>assoc [ nip ] assoc-reject
] unit-test
