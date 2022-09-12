namespace Git.Test

open System.IO.Abstractions
open NUnit.Framework
open FsUnitTyped
open Git

[<TestFixture>]
module TestPack =

    [<Test>]
    [<Explicit "Hits a real filesystem, only intended to work while developing">]
    let ``Example`` () =
        let fs = FileSystem ()

        let fi =
            fs.FileInfo.FromFileName
                "/Users/patrick/Documents/GitHub/stable-diffusion/.git/objects/pack/pack-871a8f18e20fa6104dbd769a07ca12f832048d00.pack"

        let index =
            fs.FileInfo.FromFileName
                "/Users/patrick/Documents/GitHub/stable-diffusion/.git/objects/pack/pack-871a8f18e20fa6104dbd769a07ca12f832048d00.idx"
            |> PackFile.readIndex

        let objects = PackFile.readAll fi index
        ()

    [<Test>]
    [<Explicit "Hits a real filesystem, only intended to work while developing">]
    let ``Look up a specific object`` () =
        let fs = FileSystem ()

        let fi =
            fs.FileInfo.FromFileName
                "/Users/patrick/Documents/GitHub/stable-diffusion/.git/objects/pack/pack-871a8f18e20fa6104dbd769a07ca12f832048d00.pack"

        let indexFile =
            fs.FileInfo.FromFileName
                "/Users/patrick/Documents/GitHub/stable-diffusion/.git/objects/pack/pack-871a8f18e20fa6104dbd769a07ca12f832048d00.idx"

        let desiredObject = Hash.ofString "1c4bb25a779f34d86b2d90e584ac67af91bb1303"

        let object, name, _metadata =
            PackFile.locateObject desiredObject indexFile fi
            |> Option.get
            |> function
                | PackObject.Object (Object.Blob b, name, metadata) -> b, name, metadata
                | _ -> failwith "unexpected"

        name |> shouldEqual desiredObject

        System.IO.File.WriteAllBytes ("/Users/patrick/Documents/GitHub/stable-diffusion/foo2.txt", object)
        ()
