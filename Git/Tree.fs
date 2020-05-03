namespace Git

open System
open System.IO
open System.Text
open Git.Internals

type TreeEntry =
    {
        Mode : int
        Name : string
        Hash : Hash
    }
    override this.ToString () =
        sprintf "%i %s %O" this.Mode this.Name this.Hash

[<RequireQualifiedAccess>]
module Tree =

    /// emits a byte array because the header needs to know a length
    let encode (tree : TreeEntry list) : byte [] =
        // This is a bit odd, we should probably emit the stream in a streamy way
        // rather than constructing the whole thing
        let b = StringBuilder ()
        for t in tree do
            b.Append (sprintf "%i %s%c" t.Mode t.Name (char 0))
            |> ignore
            let (Hash h) = t.Hash
            let hashStr = String(h |> List.toArray |> Array.map char)
            b.Append (hashStr)
            |> ignore

        b.ToString().ToCharArray ()
        |> Array.map byte

    /// Given a stream seeked to the point where we should start consuming,
    /// decode as a tree object.
    let decode (b : byte array) : TreeEntry list =
        use b = new MemoryStream(b)

        let stripRow () : TreeEntry option =
            let mode = Stream.consumeTo b 32uy
            match mode with
            | None -> None
            | Some mode ->
            let name = Stream.consumeTo b 0uy
            match name with
            | None -> failwith "Stream ended before we could consume a name"
            | Some name ->
            let hash = Stream.consume b 20
            {
                Mode = mode |> Array.map char |> String |> Int32.Parse
                Name = name |> Array.map char |> String
                Hash = hash |> Hash.ofBytes
            }
            |> Some

        let rec allRows () : TreeEntry seq =
            seq {
                let r = stripRow ()
                match r with
                | Some r ->
                    yield r
                    yield! allRows ()
                | None ->
                    ()
            }

        allRows ()
        |> Seq.toList
