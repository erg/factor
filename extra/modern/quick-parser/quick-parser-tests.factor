! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel modern.quick-parser sequences tools.test ;
IN: modern.quick-parser.tests

{ t } [ "(a)" qparse length 1 = ] unit-test
{ t } [ "((a))" qparse length 1 = ] unit-test
{ t } [ "( a )" qparse length 1 = ] unit-test
{ t } [ "( )" qparse length 1 = ] unit-test
{ t } [ "( a )" qparse length 1 = ] unit-test
{ t } [ "foo( a )" qparse length 1 = ] unit-test
{ t } [ "foo(a" qparse length 1 = ] unit-test
[ "(" qparse ] must-fail
[ "foo(" qparse ] must-fail


{ t } [ "{a}" qparse length 1 = ] unit-test
{ t } [ "{ }" qparse length 1 = ] unit-test
{ t } [ "{ a }" qparse length 1 = ] unit-test
{ t } [ "foo{ a }" qparse length 1 = ] unit-test
{ t } [ "foo{a" qparse length 1 = ] unit-test

{ t } [ "{{}}" qparse length 1 = ] unit-test
{ t } [ "a{{}}" qparse length 1 = ] unit-test
{ t } [ "a{{b}}" qparse length 1 = ] unit-test


[ "{" qparse ] must-fail
[ "foo{" qparse ] must-fail

{ t } [ "[a]" qparse length 1 = ] unit-test
{ t } [ "[ ]" qparse length 1 = ] unit-test
{ t } [ "[ a ]" qparse length 1 = ] unit-test
{ t } [ "foo[ a ]" qparse length 1 = ] unit-test
{ t } [ "foo[a" qparse length 1 = ] unit-test

{ t } [ "[[]]" qparse length 1 = ] unit-test
{ t } [ "a[[]]" qparse length 1 = ] unit-test
{ t } [ "a[[b]]" qparse length 1 = ] unit-test

{ t } [ "\"\"" qparse length 1 = ] unit-test
{ t } [ "foo\"\"" qparse length 1 = ] unit-test
{ t } [ "foo\"abc\"" qparse length 1 = ] unit-test

{ t } [ "''" qparse length 1 = ] unit-test
{ t } [ "'a'" qparse length 1 = ] unit-test
{ t } [ "'\a'" qparse length 1 = ] unit-test
{ t } [ "'\aasdf'" qparse length 1 = ] unit-test
