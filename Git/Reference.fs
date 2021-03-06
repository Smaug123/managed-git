namespace Git


type ReferenceUpdate =
    {
        Was : Hash option
        Now : Hash
    }

[<RequireQualifiedAccess>]
module Reference =
    let write (r : Repository) (name : string) (hash : Hash) : ReferenceUpdate =
        let refFile = r.Fs.Path.Combine ((Repository.refDir r).FullName, "heads", name) |> r.Fs.FileInfo.FromFileName
        let was =
            if refFile.Exists then
                r.Fs.File.ReadAllText refFile.FullName
                |> Hash.ofString
                |> Some
            else
                do
                    use _v = refFile.Create ()
                    ()
                None
        r.Fs.File.WriteAllText (refFile.FullName, hash.ToString ())
        {
            Was = was
            Now = hash
        }

    let lookup (r : Repository) (name : string) : Hash option =
        let refFile = r.Fs.Path.Combine ((Repository.refDir r).FullName, "heads", name) |> r.Fs.FileInfo.FromFileName
        if refFile.Exists then Some (r.Fs.File.ReadAllText refFile.FullName |> Hash.ofString) else None
