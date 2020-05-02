namespace Git.Test

open System
open System.IO
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped

open Git

[<TestFixture>]
module TestFromGitBook =

    [<Test>]
    let ``Test ch 10.2 up to Commit`` () =
        let fs = MockFileSystem ()
        let dir = fs.Path.GetTempFileName ()
        let versionDir = fs.DirectoryInfo.FromDirectoryName (dir + "_test")
        versionDir.Create()

        let repo = match Repository.init versionDir with | Ok r -> r | Error e -> failwithf "Oh no: %+A" e

        // Directory structure is correct:
        let objectsDir = fs.Path.Combine (Repository.gitDir(repo).FullName, "objects") |> fs.DirectoryInfo.FromDirectoryName
        objectsDir.EnumerateDirectories ()
        |> Seq.map (fun d -> d.Name)
        |> Seq.toList
        |> List.sort
        |> shouldEqual [
            "info"
            "pack"
        ]
        objectsDir.EnumerateFiles ("*", SearchOption.AllDirectories)
        |> shouldBeEmpty

        // Write our first object
        let h =
            "test content\n".ToCharArray ()
            |> Array.map byte
            |> Object.Blob
            |> EncodedObject.encode
            |> EncodedObject.write repo
        h
        |> shouldEqual (Hash.ofString "d670460b4b4aece5915caf5c68d12f560a9fe3e4")

        // Check that it's appeared
        objectsDir.EnumerateFiles ("*", SearchOption.AllDirectories)
        |> Seq.map (fun f -> f.Directory.Name, f.Name)
        |> Seq.exactlyOne
        |> shouldEqual ("d6", "70460b4b4aece5915caf5c68d12f560a9fe3e4")

        // Read it back in
        match EncodedObject.catFile repo h |> EncodedObject.decode with
        | Object.Blob b ->
            b
            |> Array.map char
            |> String
            |> shouldEqual "test content\n"
        | s -> failwithf "Oh no: +%A" s

        // Version control
        // TODO - add helper methods for dealing with file contents
        let h1 =
            "version 1\n".ToCharArray ()
            |> Array.map byte
            |> Object.Blob
            |> EncodedObject.encode
            |> EncodedObject.write repo
        h1
        |> shouldEqual (Hash.ofString "83baae61804e65cc73a7201a7252750c76066a30")

        let h2 =
            "version 2\n".ToCharArray ()
            |> Array.map byte
            |> Object.Blob
            |> EncodedObject.encode
            |> EncodedObject.write repo
        h2
        |> shouldEqual (Hash.ofString "1f7a7a472abf3dd9643fd615f6da379c4acb3e3a")

        objectsDir.EnumerateFiles ("*", SearchOption.AllDirectories)
        |> Seq.map (fun f -> f.Directory.Name, f.Name)
        |> Seq.toList
        |> List.sort
        |> shouldEqual [
               "1f", "7a7a472abf3dd9643fd615f6da379c4acb3e3a"
               "83", "baae61804e65cc73a7201a7252750c76066a30"
               "d6", "70460b4b4aece5915caf5c68d12f560a9fe3e4"
           ]

        match EncodedObject.catFile repo h1 |> EncodedObject.decode with
        | Object.Blob b ->
            b
            |> Array.map char
            |> String
            |> shouldEqual "version 1\n"
        | s -> failwithf "Oh no: +%A" s

        match EncodedObject.catFile repo h2 |> EncodedObject.decode with
        | Object.Blob b ->
            b
            |> Array.map char
            |> String
            |> shouldEqual "version 2\n"
        | s -> failwithf "Oh no: +%A" s

        // TODO - implement the staging area and then test it here
        // Add to the tree
        let tree1 =
            [
                {
                    Mode = 100644
                    Name = "test.txt"
                    Hash = h1
                }
            ]
            |> Object.Tree
            |> EncodedObject.encode
            |> EncodedObject.write repo
        tree1 |> shouldEqual (Hash.ofString "d8329fc1cc938780ffdd9f94e0d364e0ea74f579")

        match EncodedObject.catFile repo tree1 |> EncodedObject.decode with
        | Object.Tree t ->
            t
            |> List.exactlyOne
            |> shouldEqual {
                Mode = 100644
                Name = "test.txt"
                Hash = h1
            }
        | s -> failwithf "Oh no: +%A" s

        let newHash =
            "new file\n".ToCharArray ()
            |> Array.map byte
            |> Object.Blob
            |> EncodedObject.encode
            |> EncodedObject.write repo
        newHash |> shouldEqual (Hash.ofString "fa49b077972391ad58037050f2a75f74e3671e92")

        let tree2 =
            [
                {
                    Mode = 100644
                    Name = "new.txt"
                    Hash = newHash
                }
                {
                    Mode = 100644
                    Name = "test.txt"
                    Hash = h2
                }
            ]
            |> Object.Tree
            |> EncodedObject.encode
            |> EncodedObject.write repo
        tree2 |> shouldEqual (Hash.ofString "0155eb4229851634a0f03eb265b69f5a2d56f341")

        match EncodedObject.catFile repo tree2 |> EncodedObject.decode with
        | Object.Tree t ->
            t
            |> shouldEqual [
                {
                    Mode = 100644
                    Name = "new.txt"
                    Hash = newHash
                }
                {
                    Mode = 100644
                    Name = "test.txt"
                    Hash = h2
                }
            ]
        | s -> failwithf "Oh no: +%A" s

        // and the prefix one
        let tree3 =
            [
                {
                    Mode = 40000
                    Name = "bak"
                    Hash = tree1
                }
                {
                    Mode = 100644
                    Name = "new.txt"
                    Hash = newHash
                }
                {
                    Mode = 100644
                    Name = "test.txt"
                    Hash = h2
                }
            ]
            |> Object.Tree
            |> EncodedObject.encode
            |> EncodedObject.write repo
        tree3 |> shouldEqual (Hash.ofString "3c4e9cd789d88d8d89c1073707c3585e41b0e614")

        match EncodedObject.catFile repo tree3 |> EncodedObject.decode with
        | Object.Tree t ->
            t
            |> shouldEqual [
                {
                    Mode = 40000
                    Name = "bak"
                    Hash = tree1
                }
                {
                    Mode = 100644
                    Name = "new.txt"
                    Hash = newHash
                }
                {
                    Mode = 100644
                    Name = "test.txt"
                    Hash = h2
                }
            ]
        | s -> failwithf "Oh no: +%A" s

        // TODO: the section on commits
