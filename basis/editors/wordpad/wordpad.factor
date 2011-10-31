USING: editors io.launcher kernel io.directories.search.windows
math.parser namespaces sequences io.files arrays ;
IN: editors.wordpad

SINGLETON: wordpad
wordpad editor-class set-global

: wordpad-path ( -- path )
    \ wordpad-path get [
        "Windows NT\\Accessories"
        [ "wordpad.exe" tail? ] find-in-program-files
    ] unless* ;

M: wordpad editor-command ( file line -- command )
    drop [ wordpad-path ] dip 2array ;
