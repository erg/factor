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

{ t } [ "q[[]]" qparse length 1 = ] unit-test
{ t } [ "q[[a]]" qparse length 1 = ] unit-test
{ t } [ "q[=[a]=]" qparse length 1 = ] unit-test
{ t } [ "q[==[a]==]" qparse length 1 = ] unit-test
{ t } [ "q[====[a]====]" qparse length 1 = ] unit-test

{ t } [ "q{{}}" qparse length 1 = ] unit-test
{ t } [ "q{{a}}" qparse length 1 = ] unit-test
{ t } [ "q{={a}=}" qparse length 1 = ] unit-test
{ t } [ "q{=={a}==}" qparse length 1 = ] unit-test
{ t } [ "q{===={a}====}" qparse length 1 = ] unit-test

! Mismatched but ok
{ t } [ "q[{a]]" qparse length 1 = ] unit-test
{ t } [ "q{[a]]" qparse length 1 = ] unit-test
{ t } [ "q{[a}]" qparse length 1 = ] unit-test

[ "q[[a]=]" qparse ] must-fail
[ "q[=[a]]" qparse ] must-fail
[ "q[==[a]]" qparse ] must-fail
[ "q[[a]==]" qparse ] must-fail
[ "q[==[a]===]" qparse ] must-fail
[ "q[===[a]==]" qparse ] must-fail

[ "q{{a}=}" qparse ] must-fail
[ "q{={a}}" qparse ] must-fail
[ "q{=={a}}" qparse ] must-fail
[ "q{{a}==}" qparse ] must-fail
[ "q{=={a}===}" qparse ] must-fail
[ "q{==={a}==}" qparse ] must-fail

! Mismatched
[ "q{{a}]" qparse ] must-fail
[ "q{{a}]" qparse ] must-fail
[ "q{{a]}" qparse ] must-fail

! Exclamation words
{ } [
    ": suffix! ( a b -- c )
          ; inline" qparse drop
] unit-test

! Incomplete word definitions
[ ": suffix ;" qparse ] must-fail

! Incomplete parse
[ "CONSTANT: a (" qparse ] must-fail
[ "CONSTANT: a {" qparse ] must-fail
[ "CONSTANT: a [" qparse ] must-fail
[ "CONSTANT: a \"" qparse ] must-fail
! Regression
[ "CONSTANT: a ( " qparse ] must-fail
[ "CONSTANT: a { " qparse ] must-fail
[ "CONSTANT: a [ " qparse ] must-fail
[ "CONSTANT: a \" " qparse ] must-fail

! Strings
[ "\"abc" qparse ] must-fail
[ "\"abc\"abc" qparse ] must-fail

{ } [ 0 0 ": suffix ( a b -- c ) ;" qparse-function 3drop ] unit-test

{ }
[ "[ H{ } clone callbacks set-global ] \"alien\" add-startup-hook" qparse drop ] unit-test
