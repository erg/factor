! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: alien.strings byte-arrays kernel kernel.private lexer
parser quotations vocabs.parser words ;
IN: primitives

: make-sub-primitive ( word vocab effect -- )
    [
        create
        dup t "primitive" set-word-prop
        dup 1quotation
    ] dip define-declared ;

: make-primitive ( word vocab function effect -- )
    [
        [
            create
            dup reset-word
            dup t "primitive" set-word-prop
        ] dip
        >byte-array [ do-primitive ] curry
    ] dip define-declared ;
