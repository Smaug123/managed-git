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

        Repository.init gitDir
        |> shouldEqual (Error DirectoryDoesNotExist)

        gitDir.Create ()

        let r =
            match Repository.init gitDir with
            | Ok r -> r
            | Error r -> failwithf "Failed to init repo: %+A" r

        Repository.init gitDir
        |> shouldEqual (Error AlreadyGit)
