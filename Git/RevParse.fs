namespace Git

open System
open System.IO

[<RequireQualifiedAccess>]
module RevParse =

    /// Get the object hashes which match this start, from among the loose objects.
    let disambiguateLoose (r : Repository) (startOfHash : string) : Hash list =
        let objectDir = Repository.objectDir r

        match startOfHash.Length with
        | 0 -> objectDir.EnumerateFiles ("*", SearchOption.AllDirectories)
        | 1 ->
            if r.IsCaseSensitive then
                objectDir.EnumerateDirectories ("*", SearchOption.AllDirectories)
                |> Seq.filter (fun dir -> dir.Name.[0] = startOfHash.[0])
                |> Seq.collect (fun dir -> dir.EnumerateFiles "*")
            else
                objectDir.EnumerateDirectories (sprintf "%c*" startOfHash.[0], SearchOption.AllDirectories)
                |> Seq.collect (fun dir -> dir.EnumerateFiles "*")
        | 2 ->
            let subDir =
                r.Fs.Path.Combine (objectDir.FullName, startOfHash)
                |> r.Fs.DirectoryInfo.FromDirectoryName

            if subDir.Exists then
                subDir.EnumerateFiles ()
            else
                Seq.empty
        | _ ->
            let prefix = startOfHash.Substring (0, 2)
            let suffix = startOfHash.Substring 2

            let subDir =
                r.Fs.Path.Combine (objectDir.FullName, prefix)
                |> r.Fs.DirectoryInfo.FromDirectoryName

            if subDir.Exists then
                if r.IsCaseSensitive then
                    subDir.EnumerateFiles ()
                    |> Seq.filter (fun i -> i.Name.StartsWith (suffix, StringComparison.Ordinal))
                else
                    subDir.EnumerateFiles ()
                    |> Seq.filter (fun i -> i.Name.StartsWith (suffix, StringComparison.OrdinalIgnoreCase))
            else
                Seq.empty

        |> Seq.map (fun i -> sprintf "%s%s" i.Directory.Name i.Name)
        |> Seq.map Hash.ofString
        |> List.ofSeq

//let disambiguatePacked (r : Repository) (startOfHash : string) : Hash list =
//    let packs = PackFile.allPacks r
