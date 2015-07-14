! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: ascii io.encodings.utf8 io.files io.streams.document
kernel modern.parser modern.parser.factor multiline sequences
sets tools.test vocabs.hierarchy vocabs.loader modern.lookup
assocs ;
IN: modern.parser.factor.tests

{
    {
        T{ marray
            { texts
                V{
                    T{ doc
                        { object "{" }
                        { start T{ pos { column 0 } } }
                        { finish T{ pos { column 1 } } }
                    }
                    T{ doc
                        { object "1" }
                        { start T{ pos { column 2 } } }
                        { finish T{ pos { column 3 } } }
                    }
                    T{ doc
                        { object "2" }
                        { start T{ pos { column 4 } } }
                        { finish T{ pos { column 5 } } }
                    }
                    T{ doc
                        { object "3" }
                        { start T{ pos { column 6 } } }
                        { finish T{ pos { column 7 } } }
                    }
                    T{ doc
                        { object "}" }
                        { start T{ pos { column 8 } } }
                        { finish T{ pos { column 9 } } }
                    }
                }
            }
            { elements
                {
                    T{ mnumber { n "1" } }
                    T{ mnumber { n "2" } }
                    T{ mnumber { n "3" } }
                }
            }
        }
    }
} [ "{ 1 2 3 }" parse-modern-string ] unit-test

: check-parsed-string ( string -- ? )
    dup parse-modern-string write-parsed-string sequence= ;

: check-parsed-file ( path -- ? )
    [ utf8 file-contents ]
    [ parse-modern-file write-parsed-string ] bi sequence= ;

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

{ t } [ tools-scaffold-string parse-modern-string length 1 = ] unit-test
{ t } [ tools-scaffold-string parse-modern-string ?last mstring? ] unit-test


: check-parser-exact ( string -- ? )
    [ parse-modern-string write-parsed-string ] keep = ;

: check-parsed-exact ( string -- ? )
    parse-modern-string [ write-parsed-string parse-modern-string ] keep = ;

{ { } } [
    all-source-paths [ dup check-parsed-file ] { } map>assoc [ nip ] assoc-reject
] unit-test

{ { } } [
    all-docs-paths [ dup check-parsed-file ] { } map>assoc [ nip ] assoc-reject
] unit-test

{ { } } [
    all-tests-paths [ dup check-parsed-file ] { } map>assoc [ nip ] assoc-reject
] unit-test
