! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors combinators f.cheat f.lexer f.manifests
f.parser2 io.streams.document kernel math.parser sequences
namespaces ;
QUALIFIED-WITH: io.streams.document io
IN: f.resolver

ERROR: undefined-token token ;

TUPLE: resolved-word token word ;
C: <resolved-word> resolved-word

TUPLE: resolved-number token n ;
C: <resolved-number> resolved-number

M: lexed resolve ;

M: fword resolve
    [ [ resolve ] map ] change-body ;

M: io:token resolve
    dup text {
        { [ dup string>number ] [ <resolved-number> ] }
        ! { [ dup manifest get search-identifiers ] [ <resolved-word> ] }
        [ undefined-token ]
    } cond ;
    
