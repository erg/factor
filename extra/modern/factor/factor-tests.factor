! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel modern.factor modern.quick-parser sequences
tools.test ;
IN: modern.factor.tests

{ t } [ "( )" qparse length 1 = ] unit-test
{ t } [ "( a )" qparse length 1 = ] unit-test
{ t } [ "( a b c -- d )" qparse length 1 = ] unit-test

{ t } [ "{ }" qparse length 1 = ] unit-test
{ t } [ "{ a }" qparse length 1 = ] unit-test
{ t } [ "{ a b }" qparse length 1 = ] unit-test
{ t } [ "a{ }" qparse length 1 = ] unit-test
{ t } [ "a{ a }" qparse length 1 = ] unit-test
{ t } [ "a{ a b }" qparse length 1 = ] unit-test

{ t } [ "[ ]" qparse length 1 = ] unit-test
{ t } [ "[ a ]" qparse length 1 = ] unit-test
{ t } [ "[ a b ]" qparse length 1 = ] unit-test
{ t } [ "q[ ]" qparse length 1 = ] unit-test
{ t } [ "q[ a ]" qparse length 1 = ] unit-test
{ t } [ "q[ a b ]" qparse length 1 = ] unit-test

! Exclamation words
{ } [
    ": suffix! ( a b -- c )
          ; inline" qparse drop
] unit-test

! Incomplete word definitions
[ ": suffix ;" qparse ] must-fail

{ } [ 0 0 ": suffix ( a b -- c ) ;" qparse-function 3drop ] unit-test
