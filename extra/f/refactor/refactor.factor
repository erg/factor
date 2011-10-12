! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel vocabs.loader ;
IN: f.refactor

ERROR: old-vocabulary-does-not-exist name ;
ERROR: new-vocabulary-exists name ;
ERROR: same-vocabulary-names old new ;

: check-same-names ( old-name new-name -- old-name new-name )
    2dup = [ same-vocabulary-names ] when ;

: check-old-name ( old-name -- old-name )
    dup vocab-source-path [ old-vocabulary-does-not-exist ] unless ;
    
: check-new-name ( new-name -- new-name )
    dup vocab-source-path [ new-vocabulary-exists ] when ;

: check-vocabulary-existences ( old-name new-name -- old-name new-name )
    check-same-names
    [ check-old-name ] [ check-new-name ] bi* ;

: rename-vocabulary ( old-name new-name -- )
    check-vocabulary-existences
    2drop
    ;

ERROR: not-a-vocabulary-root name ;
    
! : move-vocabulary-root ( vocabulary-name root-name -- ) ;