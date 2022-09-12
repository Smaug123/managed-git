namespace Git.Test

open System.IO.Abstractions.TestingHelpers
open System.Text
open NUnit.Framework
open FsUnitTyped
open Git

[<TestFixture>]
module TestPack =

    [<Test>]
    let ``verify-pack snapshot test`` () =
        let fs = MockFileSystem ()

        let repoDir =
            fs.Path.Combine (fs.Path.GetTempPath (), "repo")
            |> fs.DirectoryInfo.FromDirectoryName

        repoDir.Create ()

        let repo =
            match Repository.init (BranchName "main") repoDir with
            | Ok r -> r
            | Error e -> failwithf "Oh no: %+A" e

        let ident = "fd1ac4dab39afd8713d495c8bc30ae9ea6157eea"
        let indexBytes = Resource.get (sprintf "pack-%s.idx" ident)
        let packBytes = Resource.get (sprintf "pack-%s.pack" ident)

        fs.File.WriteAllBytes (
            fs.Path.Combine (repoDir.FullName, ".git", "objects", "pack", sprintf "pack-%s.idx" ident),
            indexBytes
        )

        fs.File.WriteAllBytes (
            fs.Path.Combine (repoDir.FullName, ".git", "objects", "pack", sprintf "pack-%s.pack" ident),
            packBytes
        )

        let printer, output = Printer.makeTest ()

        Commands.VerifyPack.verifyVerbose printer repo ident

        let expected =
            Resource.get "verify-pack.txt"
            |> Encoding.ASCII.GetString
            |> fun s -> s.ReplaceLineEndings "\n"

        output ()
        |> Seq.map (sprintf "%s\n")
        |> String.concat ""
        |> shouldEqual expected
