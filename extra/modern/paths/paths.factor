! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors combinators.smart io.files kernel sequences
splitting vocabs.files vocabs.hierarchy vocabs.loader
vocabs.metadata ;
IN: modern.paths

: modern-if-available ( path -- path' )
    dup ".factor" ?tail [
        ".modern" append
        dup exists? [ nip ] [ drop ] if
    ] [
        drop
    ] if ;

ERROR: not-a-source-path path ;
: force-modern-path ( path -- path' )
    ".factor" ?tail [ ".modern" append ] [ not-a-source-path ] if ;
: modern-docs-path ( path -- path' )
    vocab-docs-path modern-if-available ;
: modern-tests-path ( path -- path' )
    vocab-tests-path modern-if-available ;
: modern-source-path ( path -- path' )
    vocab-source-path modern-if-available ;
: modern-syntax-path ( path -- path' )
    vocab-source-path ".factor" ?tail drop "-syntax.modern" append ;

: force-modern-docs-path ( path -- path' )
    vocab-docs-path force-modern-path ;
: force-modern-tests-path ( path -- path' )
    vocab-tests-path force-modern-path ;
: force-modern-source-path ( path -- path' )
    vocab-source-path force-modern-path ;

: vocabs-from ( root -- vocabs )
    "" disk-vocabs-in-root/prefix
    [ don't-load? not ] filter no-prefixes
    [ name>> ] map ;

: core-vocabs ( -- seq ) "resource:core" vocabs-from ;
: basis-vocabs ( -- seq ) "resource:basis" vocabs-from ;
: extra-vocabs ( -- seq ) "resource:extra" vocabs-from ;
: all-vocabs ( -- seq )
    [
        core-vocabs
        basis-vocabs
        extra-vocabs
    ] { } append-outputs-as ;

: filter-exists ( seq -- seq' ) [ exists? ] filter ;

: all-syntax-paths ( -- seq )
    all-vocabs [ modern-syntax-path ] map filter-exists ;

: all-source-paths ( -- seq )
    all-vocabs [ modern-source-path ] map filter-exists ;

: all-docs-paths ( -- seq )
    all-vocabs [ modern-docs-path ] map filter-exists ;

: all-tests-paths ( -- seq )
    all-vocabs [ modern-tests-path ] map filter-exists ;

: all-factor-files ( -- seq )
    [
        all-syntax-paths all-source-paths all-docs-paths all-tests-paths
    ] { } append-outputs-as ;

: vocab-names>syntax ( strings -- seq )
    [ modern-syntax-path ] map [ exists? ] filter ;

: core-syntax-files ( -- seq ) core-vocabs vocab-names>syntax ;
: basis-syntax-files ( -- seq ) basis-vocabs vocab-names>syntax ;
: extra-syntax-files ( -- seq ) extra-vocabs vocab-names>syntax ;
