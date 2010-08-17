! Copyright (C) 2010 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors db db.connections db.types db.utils kernel
make orm.persistent sequences ;
IN: orm.queries

HOOK: create-table-sql db-connection ( tuple-class -- sql )
HOOK: drop-table-sql db-connection ( tuple-class -- sql )

: create-table ( tuple-class -- )
    create-table-sql sql-command ;

: drop-table ( tuple-class -- )
    drop-table-sql sql-command ;

M: object create-table-sql
    >persistent dup table-name>>
    [
        [
            [ columns>> ] dip
            "CREATE TABLE " % %
            "(" % [ ", " % ] [
                [ column-name>> % " " % ]
                [ type>> sql-create-type>string % ]
                [ drop ] tri
                ! [ modifiers % ] bi
            ] interleave
        ] [
            drop
            find-primary-key [
                ", " %
                "PRIMARY KEY(" %
                [ "," % ] [ column-name>> % ] interleave
                ")" %
            ] unless-empty
            ");" %
        ] 2bi
    ] "" make ;

M: object drop-table-sql
    >persistent table-name>>
    "DROP TABLE " ";" surround ;
