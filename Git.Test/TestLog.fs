namespace Git.Test

open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Git
open Git.Commands

[<TestFixture>]
module TestLog =

    [<Test>]
    let ``Log can be taken`` () =
        let fs = MockFileSystem ()
        let dir = fs.Path.GetTempFileName ()
        let versionDir = fs.DirectoryInfo.FromDirectoryName (dir + "_test")
        versionDir.Create()

        let repo = match Repository.init versionDir with | Ok r -> r | Error e -> failwithf "Oh no: %+A" e

        let commits = Utils.gitBookSetup repo

        // Test the log
        Hash.ofString "95cce637b4e889eee8042515db402128bd62c0d2"
        |> Log.log repo
        |> shouldEqual commits
