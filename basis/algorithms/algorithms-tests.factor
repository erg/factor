! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: algorithms dlists iterators.input kernel math tools.test ;
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

[ { 1 2 } ] [ { 1 2 3 4 5 } [ 3 < ] take drop ] unit-test
[ 3 t ] [ { 1 2 3 4 5 } [ 3 < ] take nip iterator-read-front1 ] unit-test
[ 3 t ] [ DL{ 1 2 3 4 5 } [ 3 < ] take nip iterator-read-front1 ] unit-test
