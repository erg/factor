! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs combinators combinators.short-circuit
fry io io.files kernel math modern.lookup modern.parser
modern.parser.factor sequences sequences.extras ;
FROM: sequences => change-nth ;
IN: modern.refactor

! Renames "[" "]" to "{" "}"
: block>array ( block -- array )
    dup block? [
        dup texts>>
        [ first "{" >>object drop ]
        [ last "}" >>object drop ] bi
    ] when ;

: warn-rename-unit-test-quots ( obj -- obj )
    {
        { [ dup { [ mtoken? ] [ name>> "1array" = ] } 1&& ] [ "1array pattern detected, try using ${ } instead" print ] }
        [ ]
    } cond ;

: rename-unit-test-quots ( vocab -- )
    modern-tests-path
    dup exists? [
        dup print flush
        parse-modern-file [
            second
            dup [ munit-test? ] find-all keys [ 2 - ] map
            over
            '[
                _ [
                    warn-rename-unit-test-quots
                    block>array
                ] change-nth ] each
        ] [
            first 2drop ! write-modern-file
        ] bi
    ] [
        drop
    ] if ;
