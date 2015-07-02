! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs fry io.files kernel math modern.lookup
modern.parser modern.parser.factor sequences sequences.extras ;
FROM: sequences => change-nth ;
IN: modern.refactor

! Renames "[" "]" to "{" "}"
: block>array ( block -- array )
    dup texts>>
    [ first "{" >>object drop ]
    [ last "}" >>object drop ] bi ;

: rename-unit-test-quots ( vocab -- )
    modern-tests-path dup exists? [
        parse-modern-file [
            second
            dup [ munit-test? ] find-all keys [ 2 - ] map
            over
            '[ _ [ block>array ] change-nth ] each
        ] [
            first write-modern-file
        ] bi
    ] [
        drop
    ] if ;
