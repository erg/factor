! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: f.parser2 io.files namespaces sequences vocabs
vocabs.loader ;
IN: f.syntax

SYMBOL: syntax-vocabularies

: syntax-file-paths ( -- seq )
    vocabs [ vocab-syntax-path ] map sift
    [ exists? ] filter ;    

: parse-syntax-vocabularies ( -- seq )
    syntax-file-paths [ parse-factor-file ] map ;

: set-syntax-vocabularies ( -- )
    parse-syntax-vocabularies \ syntax-vocabularies set-global ;
