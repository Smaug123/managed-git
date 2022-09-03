namespace Git.Test

open System.IO.Abstractions
open NUnit.Framework
open FsUnitTyped
open Git

[<TestFixture>]
module TestPack =

    [<Test>]
    let ``Example`` () =
        let fs = FileSystem ()

        let fi =
            fs.FileInfo.FromFileName
                "/Users/patrick/Documents/GitHub/stable-diffusion/.git/objects/pack/pack-871a8f18e20fa6104dbd769a07ca12f832048d00.pack"

        let index =
            fs.FileInfo.FromFileName
                "/Users/patrick/Documents/GitHub/stable-diffusion/.git/objects/pack/pack-871a8f18e20fa6104dbd769a07ca12f832048d00.idx"
            |> PackFile.readIndex

        PackFile.read fi index
        ()
