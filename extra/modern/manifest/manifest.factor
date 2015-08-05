! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: byte-arrays kernel quotations sequences strings ;
IN: modern.manifest

! https://www.reddit.com/r/programming/comments/1vg4q0/which_programming_language_has_the_best_package/

! cryptosign
! yaml? factor? json?
! rest api
! any language
! containers docker
! cvs svn  git repo tags
! licenses
! semver
! distributed mirrors
! unregistering packages, aging
! source for dev, artifacts for deployment
! compare checksum on install
! easy to publish packages
! unit tests, pod docs
! nuget,cpan,npm
! which directory to download packages to
! bundle code as private package
! http://blog.versioneye.com/2015/06/22/introducing-pessimistic-mode-for-license-whitelist/
! http://spdx.org/licenses/, license whitelist

! opam http://opam.ocaml.org/
! Yum/Apt (C++ Libraries)
! Dub (D Programming Language)
! brew

! http://ed25519.cr.yp.to/









! TUPLE: module < identity-tuple
! name words
! main help
! source-loaded? docs-loaded? ;


: <definitions> ( -- pair ) { HS{ } HS{ } } [ clone ] map ;

TUPLE: source-file
{ path string }
{ top-level-form quotation }
{ checksum byte-array }
definitions
main ;

TUPLE: module
{ name string }
{ syntax-contents string }
{ source-contents string }
{ docs-contents string }
{ tests-contents string } ;

! CONSTRUCTOR: <catalog> catalog ( vocab-name -- catalog ) ;
