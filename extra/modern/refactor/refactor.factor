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
            dup [ munit-test? ] find-all keys
            ! Make sure [ ] unit-test
            [ 1 - ] map
            over '[ _ nth block? ] filter
            [ 1 - ] map
            ! Make sure looks like [ ] [ ] unit-test
            over '[ _ nth block? ] filter
            over
            '[
                _ [
                    warn-rename-unit-test-quots
                    block>array
                ] change-nth
            ] each
        ] [
            first write-modern-file
        ] bi
    ] [
        drop
    ] if ;
