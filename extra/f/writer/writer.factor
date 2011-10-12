! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors f.lexer io io.encodings.utf8 io.files
io.streams.document io.streams.string kernel sequences strings
f.recalculate ;
QUALIFIED-WITH: io.streams.document io
IN: f.writer

GENERIC: write-parsed ( object -- )

M: lexed write-parsed
    tokens>> [ write-parsed ] each ;
    
M: io:token write-parsed
    write ;

GENERIC: write-object ( obj -- )

M: sequence write-object
    [ write-parsed ] each ;

M: lexed write-object
    write-parsed ;
    
M: io:token write-object
    write-parsed ;
    
: write-factor ( object stream -- )
    <document-writer> [
        0 over rebase-line
        write-object nl
    ] with-output-stream ;
    
: write-src-file ( tree path -- )
    utf8 <file-writer> write-factor ;

: write-src-string ( tree -- obj )
    <string-writer> [ write-factor ] keep >string ;