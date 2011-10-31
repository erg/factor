USING: definitions kernel parser words sequences math.parser
namespaces editors io.launcher windows.shell32 io.files
io.directories.search.windows strings unicode.case make ;
IN: editors.editpadlite

SINGLETON: editpadlite
editpadlite editor-class set-global

: editpadlite-path ( -- path )
    \ editpadlite-path get-global [
        "JGsoft" [ >lower "editpadlite.exe" tail? ] find-in-program-files
        [ "editpadlite.exe" ] unless*
    ] unless* ;

M: editpadlite editor-command
    drop
    [ editpadlite-path , , ] { } make ;
