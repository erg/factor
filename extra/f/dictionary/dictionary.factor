! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: assocs kernel namespaces sequences strings vocabs ;
IN: f.dictionary

SYMBOL: dictionary
dictionary [ H{ } clone ] initialize

GENERIC: lookup-vocabulary ( obj -- vocabulary/f )
M: string lookup-vocabulary dictionary get at ;

GENERIC: lookup-words ( obj -- seq/f )
M: string lookup-words vocab words ;

: all-vocabularies ( -- seq ) dictionary get keys ;
: all-words ( -- seq ) dictionary get values [ words ] map concat ;

