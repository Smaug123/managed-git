namespace Git.Test

open System
open System.IO.Abstractions.TestingHelpers
open System.Text
open NUnit.Framework
open FsUnitTyped
open FsCheck
open Git

[<TestFixture>]
module TestObject =

    let private intToChar (i : int) (upper : bool) : char =
        if i < 10 then
            (byte i + byte '0')
        else
            (byte i - 10uy + byte (if upper then 'A' else 'a'))
        |> char

    let hashPrefixGenerator (len : byte) =
        gen {
            let! n = Gen.choose (0, int len)
            let! c = Gen.listOfLength n (Gen.zip (Gen.choose (0, 15)) (Gen.choose (0, 1) |> Gen.map (fun i -> i = 0)))

            let ans =
                c
                |> List.map (fun (i, u) -> intToChar i u)
                |> Array.ofList

            return String ans
        }

    let prefixesOf (s : string) : Gen<string> =
        Gen.choose (0, s.Length)
        |> Gen.map (fun i -> s.Substring (0, i))

    [<Test>]
    let ``prefixesOf generates prefixes`` () =
        let property (s1 : string, pref : string) = s1.StartsWith pref

        let gen =
            gen {
                let! s =
                    Arb.Default.String().Generator
                    |> Gen.filter (fun i -> not <| Object.ReferenceEquals (i, null))

                let! pref = prefixesOf s
                return (s, pref)
            }

        property
        |> Prop.forAll (Arb.fromGen gen)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Can look up a partial hash`` () =
        let fs = MockFileSystem ()
        let dir = fs.Path.GetTempFileName ()
        let versionDir = fs.DirectoryInfo.FromDirectoryName (dir + "_test")
        versionDir.Create ()

        let repo =
            match Repository.init versionDir with
            | Ok r -> r
            | Error e -> failwithf "Oh no: %+A" e

        let h =
            "test content\n"
            |> Encoding.ASCII.GetBytes
            |> Object.Blob
            |> EncodedObject.encode
            |> EncodedObject.write repo

        let expected = "d670460b4b4aece5915caf5c68d12f560a9fe3e4"
        let expectedHash = Hash.ofString expected
        h |> shouldEqual (Hash.ofString expected)

        let property (prefix : string) : bool =
            if expected.StartsWith prefix then
                Object.disambiguate repo prefix = [ expectedHash ]
            else
                Object.disambiguate repo prefix = []

        property
        |> Prop.forAll (Arb.fromGen (hashPrefixGenerator 40uy))
        |> Check.QuickThrowOnFailure

        property
        |> Prop.forAll (Arb.fromGen (prefixesOf expected))
        |> Check.QuickThrowOnFailure
