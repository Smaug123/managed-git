namespace Git.Test

open Git
open NUnit.Framework
open FsUnitTyped
open System
open System.IO.Abstractions.TestingHelpers

[<TestFixture>]
module TestObject =
    [<Test>]
    let hashFromDocs () =
        let t = "what is up, doc?".ToCharArray () |> Array.map byte
        let b = Blob.create t

        Object.hash b
        |> Hash.toString
        |> shouldEqual "bd9dbf5aae1a3862dd1526723246b20206e5fc37"

    [<Test>]
    let writeFromDocs () =
        let t = "what is up, doc?".ToCharArray () |> Array.map byte
        let b = Blob.create t

        let fs = MockFileSystem ()
        let dir = fs.Path.GetTempFileName ()
        let gitDir = fs.DirectoryInfo.FromDirectoryName (dir + "_test")
        gitDir.Create()

        let repo = match Repository.init gitDir with | Ok r -> r | Error e -> failwithf "Oh no: %+A" e

        Object.write repo b

        let backIn = Object.catFile repo (Object.hash b)
        backIn.Content
        |> Array.map char
        |> String
        |> shouldEqual "what is up, doc?"
