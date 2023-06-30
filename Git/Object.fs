namespace Git

type Object =
    | Blob of byte array
    | Tree of TreeEntry list
    | Commit of CommitEntry
    | Tag of TagEntry

    override this.ToString () =
        match this with
        | Blob b -> sprintf "blob: %+A" b
        | Tree t ->
            t
            |> List.map (fun i -> i.ToString ())
            |> String.concat "\n"
            |> sprintf "tree:\n%+A"
        | Commit c -> sprintf "commit:\n%O" c
        | Tag t -> sprintf "tag:\n%O" t

[<RequireQualifiedAccess>]
module Object =

    let getType (o : Object) : ObjectType =
        match o with
        | Object.Blob _ -> ObjectType.Blob
        | Object.Tag _ -> ObjectType.Tag
        | Object.Tree _ -> ObjectType.Tree
        | Object.Commit _ -> ObjectType.Commit
