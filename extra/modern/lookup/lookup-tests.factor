! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: modern.lookup sequences tools.test vocabs ;
IN: modern.lookup.tests

! { t } [ "math" test-namespace ] unit-test
! { t } [ "sequences" test-namespace ] unit-test
! { t } [ "arrays" test-namespace ] unit-test
! { t } [ "vectors" test-namespace ] unit-test

! "io.encodings.utf16n" require

! { t } [ core-vocabs failing-namespaces empty? ] unit-test
! { t } [ basis-vocabs failing-namespaces empty? ] unit-test
! { t } [ extra-vocabs failing-namespaces empty? ] unit-test

! { t } [ core-vocabs failing-namespaces2 empty? ] unit-test
! { t } [ basis-vocabs failing-namespaces2 empty? ] unit-test
! { t } [ extra-vocabs failing-namespaces2 empty? ] unit-test
