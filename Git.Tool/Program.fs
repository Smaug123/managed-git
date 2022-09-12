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
        | [| "verify-pack" ; "-v" ; path |] ->
            let verification = VerifyPack.verify repo path
            printfn "%s" (string<PackVerification> verification)
            0
        | _ ->
            failwith "unrecognised args"