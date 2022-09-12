namespace Git.Test

open System.Text
open Git
open FsUnitTyped
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

[<RequireQualifiedAccess>]
module Utils =

    let gitBookSetup (repo : Repository) : Map<Hash, CommitEntry> =
        let h1 =
            "version 1\n"
            |> Encoding.ASCII.GetBytes
            |> Object.Blob
            |> EncodedObject.encode
            |> EncodedObject.write repo

        h1
        |> shouldEqual (Hash.ofString "83baae61804e65cc73a7201a7252750c76066a30")

        let h2 =
            "version 2\n"
            |> Encoding.ASCII.GetBytes
            |> Object.Blob
            |> EncodedObject.encode
            |> EncodedObject.write repo

        h2
        |> shouldEqual (Hash.ofString "1f7a7a472abf3dd9643fd615f6da379c4acb3e3a")

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

        tree1
        |> shouldEqual (Hash.ofString "d8329fc1cc938780ffdd9f94e0d364e0ea74f579")

        let newHash =
            "new file\n"
            |> Encoding.ASCII.GetBytes
            |> Object.Blob
            |> EncodedObject.encode
            |> EncodedObject.write repo

        newHash
        |> shouldEqual (Hash.ofString "fa49b077972391ad58037050f2a75f74e3671e92")

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

        tree2
        |> shouldEqual (Hash.ofString "0155eb4229851634a0f03eb265b69f5a2d56f341")

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

        tree3
        |> shouldEqual (Hash.ofString "3c4e9cd789d88d8d89c1073707c3585e41b0e614")

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
                Parents = []
                Tree = tree1
                GpgSignature = None
            }
            |> Object.Commit

        let c1Hash =
            commit1
            |> EncodedObject.encode
            |> EncodedObject.write repo

        c1Hash
        |> Hash.toString
        |> shouldEqual "70d4408b5020e81d19906d6abdd87a73233ebf34"

        let commit2 =
            {
                Committer = scott
                Author = scott
                CommitMessage = "Second commit\n"
                Parents = [ c1Hash ]
                GpgSignature = None
                Tree = tree2
            }
            |> Object.Commit

        let c2Hash =
            commit2
            |> EncodedObject.encode
            |> EncodedObject.write repo

        c2Hash
        |> Hash.toString
        |> shouldEqual "1513b13a72f5277252cfce4ed0eda0620aca2f6a"

        let commit3 =
            {
                Committer = scott
                Author = scott
                CommitMessage = "Third commit\n"
                Parents = [ c2Hash ]
                Tree = tree3
                GpgSignature = None
            }
            |> Object.Commit

        let c3Hash =
            commit3
            |> EncodedObject.encode
            |> EncodedObject.write repo

        c3Hash
        |> Hash.toString
        |> shouldEqual "95cce637b4e889eee8042515db402128bd62c0d2"

        [
            c1Hash,
            match commit1 with
            | Object.Commit c -> c
            | _ -> failwith ""
            c2Hash,
            match commit2 with
            | Object.Commit c -> c
            | _ -> failwith ""
            c3Hash,
            match commit3 with
            | Object.Commit c -> c
            | _ -> failwith ""
        ]
        |> Map.ofList
