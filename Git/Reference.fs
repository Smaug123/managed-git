namespace Git

open System
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
        let lookup (name : string) =
            let refFile =
                r.Fs.Path.Combine ((Repository.gitDir r).FullName, name)
                |> r.Fs.FileInfo.FromFileName

            try
                r.Fs.File.ReadAllText refFile.FullName
                |> String.chopEnd "\n"
                |> Hash.ofString
                |> Some
            with
            | :? FileNotFoundException
            | :? DirectoryNotFoundException -> None

        seq {
            yield name
            yield r.Fs.Path.Combine ("refs", name)
            yield r.Fs.Path.Combine ("refs", "tags", name)
            yield r.Fs.Path.Combine ("refs", "heads", name)
            yield r.Fs.Path.Combine ("refs", "remotes", name)
            yield r.Fs.Path.Combine ("refs", "remotes", name, "HEAD")
        }
        |> Seq.tryPick lookup
