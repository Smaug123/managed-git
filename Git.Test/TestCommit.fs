namespace Git.Test

open System.IO.Abstractions.TestingHelpers
open Git
open NUnit.Framework
open FsUnitTyped
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

[<TestFixture>]
module TestCommit =

    [<Test>]
    let ``Round-trip a commit`` () =
        let fs = MockFileSystem ()
        let dir = fs.Path.GetTempFileName ()
        let versionDir = fs.DirectoryInfo.FromDirectoryName (dir + "_test")
        versionDir.Create()

        let repo = match Repository.init versionDir with | Ok r -> r | Error e -> failwithf "Oh no: %+A" e

        let scott =
            {
                Name = "Scott Chacon"
                Email = "schacon@gmail.com"
                DateTimezone = "-0700"
                Date = 1243040974<second>
            }

        let commit1 =
            {
                Committer = scott
                Author = scott
                CommitMessage = "First commit\n"
                Parents = [Hash.ofString "c7929fc1cc938780ffdd9f94e0d364e0ea74f210"]
                Tree = Hash.ofString "d8329fc1cc938780ffdd9f94e0d364e0ea74f579"
            }
            |> Object.Commit

        let h =
            EncodedObject.encode commit1
            |> EncodedObject.write repo

        let c =
            EncodedObject.catFile repo h
            |> EncodedObject.decode

        c |> shouldEqual commit1
