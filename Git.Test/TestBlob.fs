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

        Object.Blob t
        |> EncodedObject.encode
        |> EncodedObject.hash
        |> Hash.toString
        |> shouldEqual "bd9dbf5aae1a3862dd1526723246b20206e5fc37"

    [<Test>]
    let writeFromDocs () =
        let t = "what is up, doc?".ToCharArray () |> Array.map byte
        let b =
            Object.Blob t
            |> EncodedObject.encode

        let fs = MockFileSystem ()
        let dir = fs.Path.GetTempFileName ()
        let gitDir = fs.DirectoryInfo.FromDirectoryName (dir + "_test")
        gitDir.Create()

        let repo = match Repository.init gitDir with | Ok r -> r | Error e -> failwithf "Oh no: %+A" e

        b
        |> EncodedObject.write repo

        let backIn =
            EncodedObject.catFile repo (EncodedObject.hash b)
            |> EncodedObject.decode
        match backIn with
        | Object.Blob b ->
            b
            |> Array.map char
            |> String
            |> shouldEqual "what is up, doc?"
        | _ -> failwithf "Oh no: %+A" backIn
