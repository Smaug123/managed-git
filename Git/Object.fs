namespace Git

type Object =
    | Blob of byte array
    | Tree of TreeEntry list

[<RequireQualifiedAccess>]
module Object =
    do ()
