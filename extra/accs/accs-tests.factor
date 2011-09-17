! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accs kernel math tools.test ;
IN: accs.tests

<<
<<
TUPLE: abcd a b c d ;
TUPLE: dcba d c b a ;
>>

<<
\ abcd define-tuple-class-accessors
\ dcba define-tuple-class-accessors
>>
>>

[ 1 ] [ T{ abcd f 1 2 3 4 } get-abcd: a ] unit-test
[ T{ abcd f 1 2 3 7 } ] [ T{ abcd f 1 2 3 4 } 7 set-abcd: d ] unit-test

[ T{ abcd f 1 2 3 11 } ] [ T{ abcd f 1 2 3 4 } [ 7 + ] change-abcd: d ] unit-test

[ { 1 4 } ] [ T{ abcd f 1 2 3 4 } get-abcd{ a d } ] unit-test
[ 2 3 ] [ T{ abcd f 1 2 3 4 } get-abcd[ b c ] ] unit-test

[ T{ abcd f 5 2 3 6 } ] [ T{ abcd f 1 2 3 4 } { 5 6 } set-abcd{ a d } ] unit-test
[ T{ abcd f 1 5 6 4 } ] [ T{ abcd f 1 2 3 4 } 5 6 set-abcd[ b c ] ] unit-test

! Fail type check
[ T{ abcd f 1 2 3 4 } get-dcba: a ] must-fail
[ T{ abcd f 1 2 3 4 } 7 set-dcba: d ] must-fail
[ T{ abcd f 1 2 3 4 } [ 7 + ] change-dcba: d ] must-fail
[ T{ abcd f 1 2 3 4 } get-dcba{ a d } ] must-fail
[ T{ abcd f 1 2 3 4 } get-dcba[ b c ] ] must-fail
[ T{ abcd f 1 2 3 4 } { 5 6 } set-dcba{ a d } ] must-fail
[ T{ abcd f 1 2 3 4 } 5 6 set-dcba[ b c ] ] must-fail

! Fail stack order
[ 8 get-abcd: a ] must-fail
[ T{ abcd f 1 2 3 4 } 7 swap set-abcd: d ] must-fail
[ T{ abcd f 1 2 3 4 } [ 7 + ] swap change-abcd: d ] must-fail
[ 3 get-abcd{ a d } ] must-fail
[ 5 get-abcd[ b c ] ] must-fail
[ T{ abcd f 1 2 3 4 } { 5 6 } swap set-abcd{ a d } ] must-fail
[ T{ abcd f 1 2 3 4 } 5 6 rot set-abcd[ b c ] ] must-fail
