namespace Git

type Object =
    | Blob of byte array
    | Tree of TreeEntry list
    | Commit of CommitEntry

    override this.ToString () =
        match this with
        | Blob b ->
            sprintf "blob: %+A" b
        | Tree t ->
            t
            |> List.map (fun i -> i.ToString ())
            |> String.concat "\n"
            |> sprintf "tree:\n%+A"
        | Commit c ->
            sprintf "commit:\n%O" c
