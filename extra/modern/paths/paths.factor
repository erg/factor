! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors combinators.smart io.files kernel sequences
splitting vocabs.files vocabs.hierarchy vocabs.loader
vocabs.metadata sets ;
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
    [ don't-load? not ] reject
    no-prefixes
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

: modern-source-paths ( names -- paths )
    [ modern-source-path ] map filter-exists ;
: modern-docs-paths ( names -- paths )
    [ modern-docs-path ] map filter-exists ;
: modern-tests-paths ( names -- paths )
    [ modern-tests-path ] map filter-exists ;

: all-source-paths ( -- seq )
    all-vocabs modern-source-paths ;

: all-docs-paths ( -- seq )
    all-vocabs modern-docs-paths ;

: all-tests-paths ( -- seq )
    all-vocabs modern-tests-paths ;

: all-factor-files ( -- seq )
    [
        all-syntax-paths all-source-paths all-docs-paths all-tests-paths
    ] { } append-outputs-as
    {
        "resource:core/vocabs/loader/test/a/a.factor"
        "resource:core/vocabs/loader/test/b/b.factor"
        "resource:core/vocabs/loader/test/c/c.factor"
        "resource:basis/windows/shell32/shell32.factor"
        "resource:basis/windows/com/com-tests.factor"
        "resource:basis/windows/com/com.factor"
        "resource:basis/windows/com/syntax/syntax-syntax.modern"
        "resource:basis/windows/com/syntax/syntax-docs.factor"
        "resource:basis/windows/com/syntax/syntax.factor"
        "resource:basis/windows/com/wrapper/wrapper-docs.factor"
        "resource:basis/windows/directx/d2d1/d2d1.factor"
        "resource:basis/windows/directx/d3d10_1/d3d10_1.factor"
        "resource:basis/windows/directx/d3d10_1shader/d3d10_1shader.factor"
        "resource:basis/windows/directx/d3d10/d3d10.factor"
        "resource:basis/windows/directx/d3d10effect/d3d10effect.factor"
        "resource:basis/windows/directx/d3d10misc/d3d10misc.factor"
        "resource:basis/windows/directx/d3d10shader/d3d10shader.factor"
        "resource:basis/windows/directx/d3d11shader/d3d11shader.factor"
        "resource:basis/windows/directx/d3d9/d3d9.factor"
        "resource:basis/windows/directx/d3d11/d3d11.factor"
        "resource:basis/windows/directx/d3dcsx/d3dcsx.factor"
        "resource:basis/windows/directx/d3dx10core/d3dx10core.factor"
        "resource:basis/windows/directx/d3dx10mesh/d3dx10mesh.factor"
        "resource:basis/windows/directx/d3dx11core/d3dx11core.factor"
        "resource:basis/windows/directx/d3dx9anim/d3dx9anim.factor"
        "resource:basis/windows/directx/d3dx9core/d3dx9core.factor"
        "resource:basis/windows/directx/d3dx9effect/d3dx9effect.factor"
        "resource:basis/windows/directx/d3dx9math/d3dx9math.factor"
        "resource:basis/windows/directx/d3dx9mesh/d3dx9mesh.factor"
        "resource:basis/windows/directx/d3dx9shader/d3dx9shader.factor"
        "resource:basis/windows/directx/d3dx9xof/d3dx9xof.factor"
        "resource:basis/windows/directx/dinput/dinput.factor"
        "resource:basis/windows/directx/dwrite/dwrite.factor"
        "resource:basis/windows/directx/dxfile/dxfile.factor"
        "resource:basis/windows/directx/dxgi/dxgi.factor"
        "resource:basis/windows/directx/xact3/xact3.factor"
        "resource:basis/windows/directx/xapo/xapo.factor"
        "resource:basis/windows/directx/xaudio2/xaudio2.factor"
        "resource:basis/windows/shell32/shell32.factor"
        ! "resource:basis/game/input/gtk/gtk.factor"
        ! "resource:basis/libc/linux/linux.factor"
    } diff [ ".modern" tail? ] reject ;

: vocab-names>syntax ( strings -- seq )
    [ modern-syntax-path ] map [ exists? ] filter ;

: core-syntax-files ( -- seq ) core-vocabs vocab-names>syntax ;
: basis-syntax-files ( -- seq ) basis-vocabs vocab-names>syntax ;
: extra-syntax-files ( -- seq ) extra-vocabs vocab-names>syntax ;

: core-source-files ( -- seq ) core-vocabs modern-source-paths ;
: basis-source-files ( -- seq ) basis-vocabs modern-source-paths ;
: extra-source-files ( -- seq ) extra-vocabs modern-source-paths ;
