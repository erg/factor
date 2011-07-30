! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays classes.tuple classes.tuple.parser
combinators effects f.lexer f.manifests f.namespaces f.parser2
f.vocabularies fry generalizations kernel make sequences words ;
QUALIFIED: parser
QUALIFIED: f.words
IN: f.cheat

<<
: define-token ( class superclass slots -- )
    [ [ drop \ lexed ] dip define-tuple-class ]
    [
        [ 2drop name>> "<" ">" surround parser:create-in ]
        [ nip length swap '[ [ pop-parsed ] _ ndip _ boa ] ]
        [ 2drop [ all-slots rest [ name>> ] map ] [ name>> 1array ] bi <effect> ] 3tri define-inline
    ] 3bi ;
>>
<<
SYNTAX: TOKEN:
    parse-tuple-definition
    define-token ;
>>

TOKEN: @main name ;

TUPLE: identifier-stack-effect identifier stack-effect ;
C: <identifier-stack-effect> identifier-stack-effect

TOKEN: @stack-effect in out ;

TOKEN: @help name stack-effect body ;

TOKEN: @word name stack-effect body ;

TOKEN: @local-word name stack-effect body ;

TOKEN: @method object name stack-effect body ;

TOKEN: @local-method object name stack-effect body ;

TOKEN: @macro name stack-effect body ;

TOKEN: @local-macro name stack-effect body ;

TOKEN: @hook name variable stack-effect ;

TOKEN: @inline ;

TOKEN: @recursive ;

TOKEN: @flushable ;

TOKEN: @foldable ;

TOKEN: @begin-private ;

TOKEN: @end-private ;

TOKEN: @instance instance mixin ;

TOKEN: @using vocabularies ;

TOKEN: @unuse vocabulary ;

TOKEN: @in vocabulary ;

TOKEN: @predicate name superclass stack-effect body ;

TOKEN: @mixin name ;

TOKEN: @math name stack-effect ;

TOKEN: @memo name stack-effect body ;

TOKEN: @local-memo name stack-effect body ;

TOKEN: @generic name stack-effect ;

TOKEN: @generic# name arity stack-effect ;

TOKEN: @constructor name class stack-effect ;

TOKEN: @long-constructor name stack-effect body ;

TOKEN: @symbols sequence ;

TOKEN: @singletons sequence ;

TOKEN: @error name slots ;

TOKEN: @union name members ;

TOKEN: @slot name ;

TOKEN: @hashtable object ;

TOKEN: @array object ;

TOKEN: @vector object ;

TOKEN: @quotation object ;

TOKEN: @byte-array object ;

TOKEN: @hex object ;

TOKEN: @literal object ;

TOKEN: @qualified vocabulary ;

TOKEN: @qualified-with vocabulary prefix ;

TOKEN: @from vocabulary words ;

TOKEN: @forget name ;

TOKEN: @rename word vocabulary new-name ;

TOKEN: @exclude vocabulary words ;

TOKEN: @defer name ;

TOKEN: @char ch ;

TOKEN: @tuple name superclass slots ;

TOKEN: @boa-tuple name slots ;

TOKEN: @assoc-tuple name slots ;

TOKEN: @function-alias alias return name parameters ;

TOKEN: @function return name parameters ;

TOKEN: @struct name slots ;

TOKEN: @gl-function return name obj parameters ;

TOKEN: @callback return name parameters ;

TOKEN: @typedef old new ;

TOKEN: @ctype name ;

TOKEN: @alias new old ;

TOKEN: @library name ;

TOKEN: @parse-time code ;

TOKEN: @constant name value ;

TOKEN: @syntax name body ;

TOKEN: @functor-syntax name body ;

TOKEN: @locals-assignment identifiers ;

TOKEN: @literal-syntax word ;

TOKEN: @literal-quotation objects ;

TOKEN: @literal-array objects ;

TOKEN: @let quotation ;

TOKEN: @lambda bindings quotation ;

TOKEN: @flags objects ;

TOKEN: @postponed word ;

TOKEN: @article name title objects ;

TOKEN: @about name ;

TOKEN: @call stack-effect ;

TOKEN: @execute stack-effect ;

TOKEN: @ebnf name text ;
TOKEN: @functor name text ;
TOKEN: @peg name stack-effect body ;
TOKEN: @com-interface name stuff ;

TOKEN: @typed name stack-effect body ;
TOKEN: @local-typed name stack-effect body ;

TOKEN: @new-syntax name tuple slots parser ;

: function-parameters ( -- seq )
    peek-token ";" = [
        token drop f
    ] [
        "(" expect
        [
            [
                peek-token ")" = [
                    token drop
                    ";" expect f
                ] [
                    token token 2array , t
                ] if
            ] loop
        ] { } make
    ] if ;

DEFER: stack-effect

: stack-effect/token ( -- obj )
    peek-token "(" = [
        stack-effect
    ] [
        token
    ] if ;

: stack-effect-part ( -- seq )
    new-parse
    [
        peek-token {
            { [ dup "--" = ] [ drop f ] }
            { [ dup ")" = ] [ drop f ] }
            { [ dup ":" tail? ] [ drop token stack-effect/token <identifier-stack-effect> drop t ] }
            [ drop token drop t ]
        } cond
    ] loop
    pop-parsed [ push-all-parsed ] keep
    [ text ] map ;

: (stack-effect) ( -- stack-effect )
    stack-effect-part
    "--" expect
    stack-effect-part
    ")" expect 
    <@stack-effect> ;

: open-stack-effect ( -- stack-effect )
    new-parse (stack-effect) dup push-parsed ;

: stack-effect ( -- stack-effect )
    new-parse "(" expect (stack-effect) dup push-parsed ;

: optional-stack-effect ( -- stack-effect/f )
    peek-token "(" = [ stack-effect ] [ f ] if ;
    
: add-dummy-parsing-word ( namespace name quotation -- namespace )
    [ add-parsing-word ] 3keep 2drop ;

: fake-syntax-namespace ( -- namespace )
    "syntax" <namespace>
        "USING:" [
            ";" tokens-until dup [ use-namespace ] each <@using>
        ] add-dummy-parsing-word
        "USE:" [ parse-use 1array <@using> ] add-dummy-parsing-word
        "UNUSE:" [ parse-unuse <@unuse> ] add-dummy-parsing-word
        "IN:" [ token <@in> ] add-dummy-parsing-word

        "HEX:" [ token <@hex> ] add-dummy-parsing-word
        "H{" [ "}" parse-until <@hashtable> ] add-dummy-parsing-word
        "B{" [ "}" parse-until <@byte-array> ] add-dummy-parsing-word
        "V{" [ "}" parse-until <@vector> ] add-dummy-parsing-word
        "{" [ "}" parse-until <@array> ] add-dummy-parsing-word
        "[" [ "]" parse-until <@quotation> ] add-dummy-parsing-word
        ! "(" [ stack-effect ] add-dummy-parsing-word
        "$" [ token <@literal-syntax> ] add-dummy-parsing-word
        "$[" [ "]" parse-until <@literal-quotation> ] add-dummy-parsing-word
        "${" [ "}" parse-until <@literal-array> ] add-dummy-parsing-word
        "flags{" [ "}" parse-until <@flags> ] add-dummy-parsing-word
        "POSTPONE:" [ chunk <@postponed> ] add-dummy-parsing-word
        "ARTICLE:" [ parse parse body <@article> ] add-dummy-parsing-word
        "ABOUT:" [ token <@about> ] add-dummy-parsing-word
        "HELP:" [ token optional-stack-effect ";" parse-until <@help> ] add-dummy-parsing-word
        
        "TYPED:" [ token stack-effect body <@typed> ] add-dummy-parsing-word
        "TYPED::" [ token stack-effect body <@local-typed> ] add-dummy-parsing-word
        
        "[let" [ "]" parse-until <@let> ] add-dummy-parsing-word
        "[|" [ "|" tokens-until "]" parse-until <@lambda> ] add-dummy-parsing-word

        "C:" [ token token optional-stack-effect <@constructor> ] add-dummy-parsing-word

        ":>" [
            peek-token "(" = [
                "(" expect ")" tokens-until <@locals-assignment>
            ] [
                peek-token 1array <@locals-assignment>
            ] if
        ] add-dummy-parsing-word

        "MIXIN:" [ token <@mixin> ] add-dummy-parsing-word
        "INSTANCE:" [ token token <@instance> ] add-dummy-parsing-word

        "MATH:" [ token stack-effect <@math> ] add-dummy-parsing-word
        "MEMO:" [ token stack-effect ";" parse-until <@memo> ] add-dummy-parsing-word
        "MEMO::" [ token stack-effect ";" parse-until <@local-memo> ] add-dummy-parsing-word

        "GENERIC:" [ token stack-effect <@generic> ] add-dummy-parsing-word
        "GENERIC#" [ token token stack-effect <@generic#> ] add-dummy-parsing-word
        ":" [ token stack-effect body <@word> ] add-dummy-parsing-word
        "::" [ token stack-effect body <@local-word> ] add-dummy-parsing-word
        "M:" [
            token token optional-stack-effect body <@method>
        ] add-dummy-parsing-word
        "M::" [
            token token optional-stack-effect body <@local-method>
        ] add-dummy-parsing-word
        "MACRO:" [ token stack-effect body <@macro> ] add-dummy-parsing-word
        "MACRO::" [ token stack-effect body <@local-macro> ] add-dummy-parsing-word

        "MAIN:" [ token <@main> ] add-dummy-parsing-word
        "PREDICATE:" [ token "<" expect token optional-stack-effect ";" parse-until <@predicate> ]
            add-dummy-parsing-word
        "FORGET:" [ token <@forget> ] add-dummy-parsing-word

        "SYMBOLS:" [ ";" tokens-until <@symbols> ] add-dummy-parsing-word
        "SYMBOL:" [ token 1array <@symbols> ] add-dummy-parsing-word

        "SINGLETONS:" [ ";" tokens-until <@singletons> ] add-dummy-parsing-word
        "SINGLETON:" [ token 1array <@singletons> ] add-dummy-parsing-word

        "UNION:" [ token body <@union> ] add-dummy-parsing-word
        "SLOT:" [ token <@slot> ] add-dummy-parsing-word

        "ERROR:" [ token body <@error> ] add-dummy-parsing-word


        "EBNF:" [ token ";EBNF" chunks-until <@ebnf> ] add-dummy-parsing-word
        "FUNCTOR:" [ token ";FUNCTOR" tokens-until <@functor> ] add-dummy-parsing-word
        "PEG:" [ token stack-effect body <@peg> ] add-dummy-parsing-word
        "call(" [ open-stack-effect <@call> ] add-dummy-parsing-word
        "execute(" [ open-stack-effect <@execute> ] add-dummy-parsing-word
        "inline" [ <@inline> ] add-dummy-parsing-word
        "recursive" [ <@recursive> ] add-dummy-parsing-word
        "flushable" [ <@flushable> ] add-dummy-parsing-word
        "foldable" [ <@foldable> ] add-dummy-parsing-word
        "<PRIVATE" [ <@begin-private> ] add-dummy-parsing-word
        "PRIVATE>" [ <@end-private> ] add-dummy-parsing-word

        "\\" [ token <@literal> ] add-dummy-parsing-word
        "FROM:" [ token "=>" expect body <@from> ] add-dummy-parsing-word
        "EXCLUDE:" [ token "=>" expect body <@exclude> ] add-dummy-parsing-word
        "RENAME:" [ token token "=>" expect token <@rename> ] add-dummy-parsing-word
        "QUALIFIED:" [ token <@qualified> ] add-dummy-parsing-word
        "QUALIFIED-WITH:" [ token token <@qualified-with> ] add-dummy-parsing-word

        "DEFER:" [ token <@defer> ] add-dummy-parsing-word
        "CHAR:" [ chunk <@char> ] add-dummy-parsing-word
        "CONSTANT:" [ token parse <@constant> ] add-dummy-parsing-word

        "FUNCTION:" [
            token token function-parameters <@function>
        ] add-dummy-parsing-word

        "FUNCTION-ALIAS:" [
            token token token function-parameters <@function-alias>
        ] add-dummy-parsing-word

        "CALLBACK:" [
            token token function-parameters <@callback>
        ] add-dummy-parsing-word

        "GL-FUNCTION:" [
            token token parse function-parameters <@gl-function>
        ] add-dummy-parsing-word

        "<<" [ ">>" parse-until <@parse-time> ] add-dummy-parsing-word

        "TYPEDEF:" [ parse token <@typedef> ] add-dummy-parsing-word
        "STRUCT:" [
            token ";" parse-until <@struct>
        ] add-dummy-parsing-word

        "SYNTAX:" [ chunk ";" parse-until <@syntax> ] add-dummy-parsing-word
        
        "NEW-SYNTAX:" [ token token parse parse ";" expect <@new-syntax> ] add-dummy-parsing-word
        
        "FUNCTOR-SYNTAX:" [ chunk ";" parse-until <@functor-syntax> ] add-dummy-parsing-word

        "HOOK:" [ token token stack-effect <@hook> ] add-dummy-parsing-word

        "C-TYPE:" [ token <@ctype> ] add-dummy-parsing-word
        "LIBRARY:" [ token <@library> ] add-dummy-parsing-word
        "ALIAS:" [ token token <@alias> ] add-dummy-parsing-word
        "TUPLE:" [
            token
            peek-token "<" = [
                token drop token
            ] [
                f
            ] if
            [
                [
                    token
                    dup ";" = [
                        drop f
                    ] [
                        dup "{" = [
                            drop "}" tokens-until ,
                        ] [
                            ,
                        ] if t
                    ] if
                ] loop
            ] { } make <@tuple>
        ] add-dummy-parsing-word

        "T{" [
            token
            peek-token "f" = [
                token drop
                "}" parse-until <@boa-tuple>
            ] [
                [
                    [
                        token dup "}" = [
                            drop f
                        ] [
                            "{" = [
                                "}" parse-until , t
                            ] [
                                "bad tuple" throw
                            ] if
                        ] if
                    ] loop
                ] { } make <@assoc-tuple>
            ] if
        ] add-dummy-parsing-word

        "CONSTRUCTOR:" [
            token stack-effect ";" parse-until <@long-constructor>
        ] add-dummy-parsing-word
        
        "COM-INTERFACE:" [ token ";" tokens-until <@com-interface> ] add-dummy-parsing-word
    ;

M: object preload-syntax-namespaces ( manifest -- manifest )
    fake-syntax-namespace over add-namespace-to-syntax ;
