! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel modern.quick-parser sequences tools.test ;
IN: modern.quick-parser.tests

{ t } [ "( a )" quick-parse-string length 1 = ] unit-test
{ t } [ "call( a )" quick-parse-string length 1 = ] unit-test
{ t } [ "call(a" quick-parse-string length 1 = ] unit-test

[ "(" quick-parse-string ] must-fail
[ "call(" quick-parse-string ] must-fail
