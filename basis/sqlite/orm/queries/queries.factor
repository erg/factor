! Copyright (C) 2010 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators db.binders
db.statements db.types fry kernel make namespaces
nested-comments orm.persistent orm.queries sequences
sqlite.db.connections ;
IN: sqlite.orm.queries

(*
: insert-trigger-not-null ( -- string )
    [
    """
        CREATE TRIGGER fki_${table-name}_${table-id}_${foreign-table-name}_${foreign-table-id}_id
        BEFORE INSERT ON ${table-name}
        FOR EACH ROW BEGIN
            SELECT RAISE(ROLLBACK, 'insert on table "${table-name}" violates foreign key constraint "fki_${table-name}_$table-id}_${foreign-table-name}_${foreign-table-id}_id"')
            WHERE NEW.${table-id} IS NOT NULL
                AND (SELECT ${foreign-table-id} FROM ${foreign-table-name} WHERE ${foreign-table-id} = NEW.${table-id}) IS NULL;
        END;
    """ interpolate
    ] with-string-writer ;

: update-trigger ( -- string )
    [
    """
        CREATE TRIGGER fku_${table-name}_${table-id}_${foreign-table-name}_${foreign-table-id}_id
        BEFORE UPDATE ON ${table-name}
        FOR EACH ROW BEGIN
            SELECT RAISE(ROLLBACK, 'update on table "${table-name}" violates foreign key constraint "fku_${table-name}_$table-id}_${foreign-table-name}_${foreign-table-id}_id"')
            WHERE (SELECT ${foreign-table-id} FROM ${foreign-table-name} WHERE ${foreign-table-id} = NEW.${table-id}) IS NULL;
        END;
    """ interpolate
    ] with-string-writer ;

: update-trigger-not-null ( -- string )
    [
    """
        CREATE TRIGGER fku_${table-name}_${table-id}_${foreign-table-name}_${foreign-table-id}_id
        BEFORE UPDATE ON ${table-name}
        FOR EACH ROW BEGIN
            SELECT RAISE(ROLLBACK, 'update on table "${table-name}" violates foreign key constraint "fku_${table-name}_$table-id}_${foreign-table-name}_${foreign-table-id}_id"')
            WHERE NEW.${table-id} IS NOT NULL
                AND (SELECT ${foreign-table-id} FROM ${foreign-table-name} WHERE ${foreign-table-id} = NEW.${table-id}) IS NULL;
        END;
    """ interpolate
    ] with-string-writer ;

: delete-trigger-restrict ( -- string )
    [
    """
        CREATE TRIGGER fkd_${table-name}_${table-id}_${foreign-table-name}_${foreign-table-id}_id
        BEFORE DELETE ON ${foreign-table-name}
        FOR EACH ROW BEGIN
            SELECT RAISE(ROLLBACK, 'delete on table "${foreign-table-name}" violates foreign key constraint "fkd_${table-name}_$table-id}_${foreign-table-name}_${foreign-table-id}_id"')
            WHERE (SELECT ${foreign-table-id} FROM ${foreign-table-name} WHERE ${foreign-table-id} = OLD.${foreign-table-id}) IS NOT NULL;
        END;
    """ interpolate
    ] with-string-writer ;

: delete-trigger-cascade ( -- string )
    [
    """
        CREATE TRIGGER fkd_${table-name}_${table-id}_${foreign-table-name}_${foreign-table-id}_id
        BEFORE DELETE ON ${foreign-table-name}
        FOR EACH ROW BEGIN
            DELETE from ${table-name} WHERE ${table-id} = OLD.${foreign-table-id};
        END;
    """ interpolate
    ] with-string-writer ;
*)

! : can-be-null? ( -- ? ) "sql-spec" get modifiers>> [ +not-null+ = ] any? not ;

! : delete-cascade? ( -- ? ) "sql-spec" get modifiers>> { +on-delete+ +cascade+ } swap subseq? ;

! : sqlite-trigger, ( string -- ) { } { } <simple-statement> 3, ;

: sqlite-create-table ( tuple-class -- string )
    >persistent dup table-name>>
    [
        [
            [ columns>> ] dip
            "CREATE TABLE " % %
            "(" % [ ", " % ] [
                [ column-name>> % " " % ]
                [ type>> sql-create-type>string % ]
                [
                    modifiers>> sql-modifiers>string
                    [ " " % % ] unless-empty
                ] tri
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


(*
: create-sqlite-triggers ( -- )
    can-be-null? [
        insert-trigger sqlite-trigger,
        update-trigger sqlite-trigger,
    ] [
        insert-trigger-not-null sqlite-trigger,
        update-trigger-not-null sqlite-trigger,
    ] if
    delete-cascade? [
        delete-trigger-cascade sqlite-trigger,
    ] [
        delete-trigger-restrict sqlite-trigger,
    ] if ;

: create-db-triggers ( sql-specs -- )
    [ modifiers>> [ +foreign-id+ = ] deep-any? ] filter
    [
        [ class>> db-table-name "db-table" set ]
        [
            [ "sql-spec" set ]
            [ column-name>> "table-id" set ]
            [ ] tri
            modifiers>> [ [ +foreign-id+ = ] deep-any? ] filter
            [
                [ second db-table-name "foreign-table-name" set ]
                [ third "foreign-table-id" set ] bi
                create-sqlite-triggers
            ] each
        ] bi
    ] each ;
*)

M: sqlite-db-connection create-table-sql ( class -- seq )
    ! [ sqlite-create-table ] [ drop create-db-triggers ] 2bi 2array ;
    sqlite-create-table ;

: filter-tuple-values ( persistent tuple -- assoc )
    [ columns>> ] dip
    '[ _ over getter>> call( obj -- slot-value ) ] { } map>assoc
    [ nip ] assoc-filter ;

M: sqlite-db-connection insert-tuple-sql ( tuple -- object )
    [ <statement> ] dip
    [ >persistent ] [ ] bi {
        [ drop table-name>> "INSERT INTO " "(" surround add-sql ]
        [
            filter-tuple-values
            [
                keys
                [ [ column-name>> ] map ", " join ]
                [
                    length "?" <array> ", " join
                    ") values(" ");" surround
                ] bi append add-sql
            ]
            [ [ [ second ] [ first type>> ] bi <in-binder-low> ] map >>in ] bi
        ]
    } 2cleave ;
