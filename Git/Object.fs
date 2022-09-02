namespace Git

open System.IO

type Object =
    | Blob of byte array
    | Tree of TreeEntry list
    | Commit of CommitEntry

    override this.ToString () =
        match this with
        | Blob b -> sprintf "blob: %+A" b
        | Tree t ->
            t
            |> List.map (fun i -> i.ToString ())
            |> String.concat "\n"
            |> sprintf "tree:\n%+A"
        | Commit c -> sprintf "commit:\n%O" c

[<RequireQualifiedAccess>]
module Object =
    /// Get the object hashes which match this start.
    let disambiguate (r : Repository) (startOfHash : string) : Hash list =
        match startOfHash.Length with
        | 0 -> (Repository.objectDir r).EnumerateFiles ("*", SearchOption.AllDirectories)
        | 1 ->
            if r.IsCaseSensitive then
                (Repository.objectDir r).EnumerateDirectories ("*", SearchOption.AllDirectories)
                |> Seq.filter (fun dir -> dir.Name.[0] = startOfHash.[0])
                |> Seq.collect (fun dir -> dir.EnumerateFiles "*")
            else
                (Repository.objectDir r)
                    .EnumerateDirectories (sprintf "%c*" startOfHash.[0], SearchOption.AllDirectories)
                |> Seq.collect (fun dir -> dir.EnumerateFiles "*")
        | 2 ->
            let subDir =
                r.Fs.Path.Combine ((Repository.objectDir r).FullName, startOfHash)
                |> r.Fs.DirectoryInfo.FromDirectoryName

            if subDir.Exists then
                subDir.EnumerateFiles ()
            else
                Seq.empty
        | _ ->
            let prefix = startOfHash.Substring (0, 2)
            let suffix = startOfHash.Substring (2, startOfHash.Length - 2)

            let subDir =
                r.Fs.Path.Combine ((Repository.objectDir r).FullName, prefix)
                |> r.Fs.DirectoryInfo.FromDirectoryName

            if subDir.Exists then
                subDir.EnumerateFiles ()
                |> Seq.filter (fun i -> i.Name.StartsWith suffix)
            else
                Seq.empty

        |> Seq.map (fun i -> sprintf "%s%s" i.Directory.Name i.Name)
        |> Seq.map Hash.ofString
        |> List.ofSeq
