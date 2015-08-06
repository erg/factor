! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: modern.quick-parser ;
IN: modern.factor

QPARSER: in IN: token ;
QPARSER: char CHAR: raw ;
! QPARSER: use USE: token ;

! QPARSER: unuse UNUSE: token ;
! ! QPARSER: from FROM: token "=>" expect ";" raw-until ;
! ! QPARSER: exclude EXCLUDE: token "=>" expect ";" raw-until ;
! ! QPARSER: rename RENAME: raw raw "=>" expect raw ;
! QPARSER: qualified QUALIFIED: token ;
! QPARSER: qualified-with QUALIFIED-WITH: token token ;
! QPARSER: forget FORGET: token ;


QPARSER: builtin BUILTIN: existing-class body ;
