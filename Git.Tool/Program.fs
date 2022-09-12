namespace Git.Tool

open System.IO
open System.IO.Abstractions
open Git
open Git.Commands

module Program =

    [<EntryPoint>]
    let main args =
        let fs = FileSystem ()
        let printer = Printer.make ()

        let repo =
            Directory.GetCurrentDirectory ()
            |> fs.DirectoryInfo.FromDirectoryName
            |> Repository.make

        match repo with
        | None -> failwith "not in a git repo"
        | Some repo ->

        match args with
        | [| "verify-pack" ; "-v" ; hash |] ->
            VerifyPack.verifyVerbose printer repo hash
            0
        | _ -> failwith "unrecognised args"
