! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: modern.parser ;
IN: roman

PARSER: roman-op { name body }
    ROMAN-OP: token ";" parse-until ;

PARSER: roman-literal { string } ROMAN: token ;
