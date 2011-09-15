! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: debugger globals kernel math tools.test vars
vars.private ;
IN: vars.tests

! The order in which these tests are defined matters.

SYMBOL: vartest

[ ] [ vartest gunset ] unit-test
[ ] [ vartest vunset ] unit-test

[ 555 ]
[ 555 vartest [ vartest vget ] with-var ] unit-test

! Test that top of stack is available to vchange's quot
[ 666 ]
[ 555 vartest [ 111 vartest [ + dup ] vchange ] with-var ] unit-test

[ "vartest1234" vget ]
[ T{ undefined-variable { variable "vartest1234" } } = ] must-fail-with

[ 555 vartest [ vartest vget ] with-var vartest vget ]
[ T{ undefined-variable { variable vartest } } = ] must-fail-with

[ 666 ] [
    555 vartest [
        vartest [ 111 + ] vchange
        vartest vget
    ] with-var
] unit-test

[ f ] [
    555 vartest [
        vartest voff
        vartest vget
    ] with-var
] unit-test

[ t ] [
    555 vartest [
        vartest von
        vartest vget
    ] with-var
] unit-test

[ 666 ] [
    555 vartest [
        666 vartest [
            vartest vget
        ] with-var
    ] with-var
] unit-test

[ vartest vget ]
[ T{ undefined-variable { variable vartest } } = ] must-fail-with

[ f ] [ vartest voff vartest vget ] unit-test
[ t ] [ vartest von vartest vget ] unit-test
[ 777 ] [ 777 vartest vset vartest vget ] unit-test

[
    vartest vunset
    vartest vget
] [ T{ undefined-variable { variable vartest } } = ] must-fail-with

[
    5 vartest [ "oops" throw ] with-var
] [ "oops" = ] must-fail-with

[
    [ 5 vartest [ "oops" throw ] with-var ] try
    vartest vget
] [ T{ undefined-variable { variable vartest } } = ] must-fail-with

[ "abc" vunset "abc" pop-var ]
[ T{ unbalanced-with-var { variable "abc" } } = ] must-fail-with
