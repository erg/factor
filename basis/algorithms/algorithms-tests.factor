! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: algorithms dlists math tools.test ;
FROM: sequences => iota ;
IN: algorithms.tests

[ { 1 4 9 } ]
[ { 1 2 3 } [ sq ] { } map-as ] unit-test

[ V{ 1 4 9 } ]
[ { 1 2 3 } [ sq ] V{ } map-as ] unit-test

[ DL{ 1 4 9 } ] [ DL{ 1 2 3 } [ sq ] DL{ } map-as ] unit-test
[ { 1 4 9 } ] [ DL{ 1 2 3 } [ sq ] { } map-as ] unit-test
[ { 0 1 4 } ] [ 3 iota [ sq ] map ] unit-test

[ 2 ] [ { 1 2 3 } [ odd? ] count ] unit-test
[ 2 ] [ V{ 1 2 3 } [ odd? ] count ] unit-test
[ 2 ] [ DL{ 1 2 3 } [ odd? ] count ] unit-test
