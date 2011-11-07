USING: help.markup help.syntax arrays io.files ;
IN: io.files.info

HELP: get-file-info
{ $values { "path" "a pathname string" } { "info" file-info } }
{ $description "Queries the file system for metadata. If " { $snippet "path" } " refers to a symbolic link, it is followed. See the article " { $link "file-types" } " for a list of metadata symbols." }
{ $errors "Throws an error if the file does not exist." } ;

HELP: get-link-info
{ $values { "path" "a pathname string" } { "info" "a file-info tuple" } }
{ $description "Queries the file system for metadata. If path refers to a symbolic link, information about the symbolic link itself is returned. If the file does not exist, an exception is thrown." } ;

{ get-file-info get-link-info } related-words

HELP: directory?
{ $values { "file-info" file-info } { "?" "a boolean" } }
{ $description "Tests if " { $snippet "file-info" } " is a directory." } ;

HELP: get-file-systems
{ $values { "array" array } }
{ $description "Returns an array of " { $link file-system-info } " objects returned by iterating the mount points and calling " { $link file-system-info } " on each." } ;

HELP: get-file-system-info
{ $values
{ "path" "a pathname string" }
{ "file-system-info" file-system-info } }
{ $description "Returns a platform-specific object describing the file-system that contains the path. The cross-platform slot is " { $slot "free-space" } "." }
{ $examples
    { $unchecked-example
        "USING: io.files.info io.pathnames math prettyprint ;"
        "IN: scratchpad"
        ""
        ": gb ( m -- n ) 30 2^ * ;"
        ""
        "home file-system-info free-space>> 100 gb < ."
        "f"
    }
} ;

ARTICLE: "io.files.info" "File system meta-data"
"File meta-data:"
{ $subsections
    get-file-info
    get-link-info
    exists?
    directory?
}
"File types:"
{ $subsections "file-types" }
"File system meta-data:"
{ $subsections
    get-file-system-info
    get-file-systems
} ;

ABOUT: "io.files.info"
