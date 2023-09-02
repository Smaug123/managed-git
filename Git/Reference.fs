namespace Git

open System.IO


type ReferenceUpdate =
    {
        Was : Hash option
        Now : Hash
    }

[<RequireQualifiedAccess>]
module Reference =
    let write (r : Repository) (name : string) (hash : Hash) : ReferenceUpdate =
        let refFile =
            r.Fs.Path.Combine ((Repository.refDir r).FullName, "heads", name)
            |> r.Fs.FileInfo.FromFileName

        let was =
            try
                r.Fs.File.ReadAllText refFile.FullName |> Some
            with :? FileNotFoundException ->
                None
            |> Option.map Hash.ofString

        r.Fs.File.WriteAllText (refFile.FullName, hash.ToString ())

        {
            Was = was
            Now = hash
        }

    let lookup (r : Repository) (name : string) : Hash option =
        let refFile =
            r.Fs.Path.Combine ((Repository.refDir r).FullName, "heads", name)
            |> r.Fs.FileInfo.FromFileName

        try
            r.Fs.File.ReadAllText refFile.FullName |> Hash.ofString |> Some
        with :? FileNotFoundException ->
            None
