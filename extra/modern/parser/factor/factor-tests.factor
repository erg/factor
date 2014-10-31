! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: ascii io.encodings.utf8 io.files io.streams.document
kernel modern.parser modern.parser.factor multiline sequences
sets tools.test vocabs.hierarchy vocabs.loader ;
IN: modern.parser.factor.tests

{
    {
        T{ marray
            { texts
                V{
                    T{ document-object { object "{" } }
                    T{ document-object
                        { position
                            T{ document-position { column 2 } }
                        }
                        { object "1" }
                    }
                    T{ document-object
                        { position
                            T{ document-position { column 4 } }
                        }
                        { object "2" }
                    }
                    T{ document-object
                        { position
                            T{ document-position { column 6 } }
                        }
                        { object "3" }
                    }
                    T{ document-object
                        { position
                            T{ document-position { column 8 } }
                        }
                        { object "}" }
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
} [ "{ 1 2 3 }" parse-source-string ] unit-test


: check-parsed-file ( path -- ? )
    [ utf8 file-contents [ blank? ] trim-tail ]
    [ parse-modern-file second write-parsed-string ] bi sequence= ;

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
    "resource:core" vocabs-in-root
    ! [ vocab? ] filter
    [ vocab-source-path ] map sift
    {
        "resource:core/vocabs/loader/test/a/a.factor"
        "resource:core/vocabs/loader/test/b/b.factor"
        "resource:core/vocabs/loader/test/c/c.factor"
    } diff
    ! [ parse-modern-file ] map
    [ check-parsed-file ] all?
] unit-test

/*
{ } [
    "resource:basis" vocabs-in-root
    [ vocab? ] filter
    [ vocab-source-path ] map sift
    {

    } diff
    [ dup . flush parse-modern-file ] map
] unit-test

*/