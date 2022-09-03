namespace Git.Test

open NUnit.Framework
open FsUnitTyped
open System.IO.Abstractions.TestingHelpers

open Git

[<TestFixture>]
module TestInit =

    [<Test>]
    let ``test initialisation`` () =
        let fs = MockFileSystem ()
        let dir = fs.Path.GetTempFileName ()
        let gitDir = fs.DirectoryInfo.FromDirectoryName (dir + "_test")

        Repository.init (BranchName "main") gitDir
        |> shouldEqual (Error DirectoryDoesNotExist)

        gitDir.Create ()

        let _ =
            match Repository.init (BranchName "main") gitDir with
            | Ok r -> r
            | Error r -> failwithf "Failed to init repo: %+A" r

        Repository.init (BranchName "main") gitDir
        |> shouldEqual (Error AlreadyGit)
