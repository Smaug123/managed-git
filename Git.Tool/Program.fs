namespace Git.Tool

open System
open System.IO
open System.IO.Abstractions
open Git

module Program =

    [<EntryPoint>]
    let main args =
        let fs = FileSystem ()

        let repo =
            Directory.GetCurrentDirectory ()
            |> fs.DirectoryInfo.FromDirectoryName
            |> Repository.make

        match repo with
        | None -> failwith "not in a git repo"
        | Some repo ->

        match args with
        | [| "verify-pack" ; "-v" ; hash |] ->
            // TODO: this is not an exact match with Git, in that `git verify-pack`
            // specifies a path to a file, not a hash's integrity to verify.
            let verification = VerifyPack.verify repo (Hash.ofString hash)
            printfn "%s" (string<PackVerification> verification)
            0
        | _ -> failwith "unrecognised args"
