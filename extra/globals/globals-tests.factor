! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: globals kernel tools.test ;
IN: globals.tests

! The order in which these tests are defined matters.

SYMBOL: gtest

[ ] [ gtest gunset ] unit-test

[ gtest gget ]
[ T{ undefined-global { global gtest } } = ] must-fail-with

[ 1 ]
[ 1 gtest gset gtest gget ] unit-test

[ t ]
[ gtest gon gtest gget ] unit-test

[ f ]
[ gtest goff gtest gget ] unit-test

[ ] [ gtest gunset ] unit-test

[ gtest gget ]
[ T{ undefined-global { global gtest } } = ] must-fail-with

