! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs constructors io.directories io.files
io.files.types io.pathnames kernel modern.parser namespaces
sequences fry continuations modern.parser.factor ;
IN: modern.loader

SYMBOL: modules
modules [ H{ } clone ] initialize

TUPLE: loader ;

TUPLE: module name paths dictionary ;

CONSTRUCTOR: module ( name -- module ) ;

SYMBOL: module-roots
module-roots [ V{ "resource:core/" "resource:basis/" "resource:extra/" } clone ] initialize

: replace-dots ( name -- name' )
    { { CHAR: . CHAR: / } }  substitute ;

: append-module ( module-root name -- path )
    replace-dots append-path ;

: ?directory-entries ( path -- seq/f )
    '[ _ directory-entries ] [ drop f ] recover ;

: filter-directories ( path -- seq/f )
    ?directory-entries [ type>> +directory+ = ] filter ;

: filter-files ( path -- seq/f )
    ?directory-entries [ type>> +regular-file+ = ] filter ;

: filter-factor ( seq -- seq' )
    [ ".factor" tail? ] filter ;

: root-name>files ( module-root name -- seq )
    append-module filter-files ;

: root-name>paths ( module-root name -- seq )
    append-module
    dup filter-files [ name>> append-path ] with map ; 

: root-name>factor-paths ( module-root name -- seq )
    root-name>paths filter-factor ;

: root-name>potential-modules ( module-root name -- seq )
    append-module
    dup directory-entries [ type>> +directory+ = ] filter
    [ name>> append-path ] with map ;

: root-name>module-path? ( module-root name -- ? )
    root-name>files empty? not ;

: root-name-subpath>path ( module-root name subpath -- path )
    [ dup append-module ] dip "-" glue ".factor" append append-path ;

: root>main-path ( module-root name -- path )
    "" root-name-subpath>path ;

: root-name>docs-path ( module-root name -- path )
    "docs" root-name-subpath>path ;

: root-name>syntax-path ( module-root name -- path )
    "syntax" root-name-subpath>path ;

: root-name>tests-path ( module-root name -- path )
    "tests" root-name-subpath>path ;

: name>paths ( name -- paths )
    [ module-roots get ] dip
    '[ _ root-name>paths ] map-find drop ;

: name>all-paths ( name -- paths )
    [ module-roots get ] dip
    '[ _ root-name>paths ] map ;

: name>factor-paths ( name -- paths )
    name>paths filter-factor ;

ERROR: module-path-conflict paths ;

ERROR: module-not-found name ;

: module-main-path ( name -- path )
    dup name>all-paths harvest [
        module-not-found
    ] [
        nip
        dup length 1 = [ module-path-conflict ] unless
    ] if-empty ;


: parse-module ( name -- module )
    module-main-path [ [ parse-file ] map ] map concat ;

GENERIC: post-parse-action ( parsed -- quotation )

M: using post-parse-action
    strings>> [ parse-module ] map ;
