USING: accessors arrays compiler.units continuations debugger
definitions eval help.crossref help.markup help.syntax
help.topics io.streams.string kernel parser sequences
tools.test words ;
IN: help.crossref.tests

[ ] [
    "IN: help.crossref.tests USING: help.syntax help.markup ; : foo ( -- ) ; HELP: foo \"foo is great\" ; ARTICLE: \"foo\" \"Foo\" { $subsection foo } ;" eval( -- )
] unit-test

[ $subsection ] [
    "foo" article-content first first
] unit-test

[ t ] [
    "foo" article-children
    "foo" "help.crossref.tests" lookup >link 1array sequence=
] unit-test

[ "foo" ] [ "foo" "help.crossref.tests" lookup article-parent ] unit-test

[ ] [
    [ "foo" "help.crossref.tests" lookup forget ] with-compilation-unit
] unit-test

[ ] [
    "IN: help.crossref.tests USING: help.syntax help.markup ; : bar ( -- ) ; HELP: bar \"bar is great\" ; ARTICLE: \"bar\" \"Bar\" { $subsection bar } ;" eval( -- )
] unit-test

[ ] [
    "IN: ayy USE: help.syntax ARTICLE: \"b\" \"B\" ;"
    <string-reader> "ayy" parse-stream drop
] unit-test

[ ] [
    "IN: azz USE: help.syntax USE: help.markup ARTICLE: \"a\" \"A\" { $subsection \"b\" } ;"
    <string-reader> "ayy" parse-stream drop
] unit-test

[ ] [
    "IN: ayy USE: help.syntax ARTICLE: \"c\" \"C\" ;"
    <string-reader> "ayy" parse-stream drop
] unit-test

[ ] [
    "IN: azz USE: help.syntax USE: help.markup ARTICLE: \"a\" \"A\" { $subsection \"c\" } ;"
    <string-reader> "ayy" parse-stream drop
] unit-test

[ ] [
    [
        "IN: azz USE: help.syntax USE: help.markup ARTICLE: \"yyy\" \"YYY\" ; ARTICLE: \"xxx\" \"XXX\" { $subsection \"yyy\" } ; ARTICLE: \"yyy\" \"YYY\" ;"
        <string-reader> "parent-test" parse-stream drop
    ] [ :1 ] recover
] unit-test

[ "xxx" ] [ "yyy" article-parent ] unit-test

ARTICLE: "crossref-test-1" "Crossref test 1"
"Hello world" ;

ARTICLE: "crossref-test-2" "Crossref test 2"
{ $markup-example { $subsection "crossref-test-1" } } ;

[ { } ] [ "crossref-test-2" >link article-children ] unit-test

[ """USING: help.syntax help.markup ; ARTICLE: "romanloop" "Foo" { $subsection "romanloop" } ;""" eval( -- ) ]
[ error>> subsection-circularity? ] must-fail-with

[ """USING: help.syntax help.markup ;
ARTICLE: "article0" "foo0000" { $subsections "article1" } ;
ARTICLE: "article1" "foo0001" { $subsections "article0" } ;
""" eval( -- )
] [ error>> subsection-circularity? ] must-fail-with

[ """USING: help.syntax help.markup ;
ARTICLE: "article2" "foo0002" { $subsections "article3" } ;
ARTICLE: "article3" "foo0003" { $subsections "article4" } ;
ARTICLE: "article4" "foo0004" { $subsections "article2" } ;
""" eval( -- )
] [ error>> subsection-circularity? ] must-fail-with
