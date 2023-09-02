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
        | [| "rev-parse" ; ref |] ->
            let parsed = RevParse.parse repo ref

            match parsed with
            | Ok h ->
                System.Console.WriteLine (Hash.toString h)
                0
            | Error (e : RevParseError) ->
                System.Console.Error.WriteLine (e.ToString ())
                1
        | [| "branch" ; branchName |] ->
            match Branch.createFromHead repo branchName with
            | Error e ->
                System.Console.Error.WriteLine (e.ToString ())
                1
            | Ok _ -> 0
        | [| "branch" ; branchName ; baseRef |] ->
            match Branch.create repo branchName baseRef with
            | Error e ->
                System.Console.Error.WriteLine (e.ToString ())
                1
            | Ok _ -> 0
        | _ -> failwith "unrecognised args"
