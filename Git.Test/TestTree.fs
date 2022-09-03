namespace Git.Test

open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped

open Git

[<TestFixture>]
module TestTree =

    [<Test>]
    let ``test tree object`` () =
        let t : TreeEntry list =
            [
                {
                    TreeEntry.Hash = Hash.ofString "a84943494657751ce187be401d6bf59ef7a2583c"
                    Name = "octopus-admin"
                    Mode = 40000
                }
                {
                    TreeEntry.Hash = Hash.ofString "14f589a30cf4bd0ce2d7103aa7186abe0167427f"
                    Name = "octopus-deployment"
                    Mode = 40000
                }
                {
                    TreeEntry.Hash = Hash.ofString "ec559319a263bc7b476e5f01dd2578f255d734fd"
                    Name = "octopus-product"
                    Mode = 40000
                }
                {
                    TreeEntry.Hash = Hash.ofString "97e5b6b292d248869780d7b0c65834bfb645e32a"
                    Name = "pom.xml"
                    Mode = 100644
                }
                {
                    TreeEntry.Hash = Hash.ofString "6e63db37acba41266493ba8fb68c76f83f1bc9dd"
                    Name = "src"
                    Mode = 40000
                }
            ]

        let b = Object.Tree t |> EncodedObject.encode

        let fs = MockFileSystem ()
        let dir = fs.Path.GetTempFileName ()
        let gitDir = fs.DirectoryInfo.FromDirectoryName (dir + "_test")
        gitDir.Create ()

        let repo =
            match Repository.init (BranchName "main") gitDir with
            | Ok r -> r
            | Error e -> failwithf "Oh no: %+A" e

        b |> EncodedObject.write repo |> ignore

        let backIn =
            EncodedObject.catFile repo (EncodedObject.hash b)
            |> EncodedObject.decode

        match backIn with
        | Object.Tree entries -> entries |> shouldEqual t
        | _ -> failwithf "Oh no: %+A" backIn
