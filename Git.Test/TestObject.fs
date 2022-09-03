namespace Git.Test

open System
open System.IO.Abstractions.TestingHelpers
open System.Runtime.InteropServices
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

    let private boolGen : Gen<bool> = Gen.choose (0, 1) |> Gen.map ((=) 1)

    let hashPrefixGenerator (len : byte) =
        gen {
            let! prefixLength = Gen.choose (0, int len)

            let! hash =
                gen {
                    let! isUpper = boolGen
                    let! hexDigit = Gen.choose (0, 15)
                    return intToChar hexDigit isUpper
                }
                |> Gen.listOfLength prefixLength

            return String (Array.ofList hash)
        }

    [<Test>]
    let ``Can look up a partial hash`` () =
        let fs = MockFileSystem ()
        let dir = fs.Path.GetTempFileName ()
        let versionDir = fs.DirectoryInfo.FromDirectoryName (dir + "_test")
        versionDir.Create ()

        let repo =
            match Repository.init (BranchName "main") versionDir with
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
            let isMatch =
                if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                    // Windows filesystem is case-insensitive
                    expected.StartsWith (prefix, StringComparison.InvariantCultureIgnoreCase)
                else
                    expected.StartsWith prefix

            if isMatch then
                Object.disambiguate repo prefix = [ expectedHash ]
            else
                Object.disambiguate repo prefix = []

        property
        |> Prop.forAll (Arb.fromGen (hashPrefixGenerator 40uy))
        |> Check.QuickThrowOnFailure

        for subStringEnd in 0 .. expected.Length - 1 do
            property expected.[0..subStringEnd]
            |> shouldEqual true

            expected.[0..subStringEnd].ToUpperInvariant ()
            |> property
            |> shouldEqual true
