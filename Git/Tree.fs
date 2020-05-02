namespace Git

open System
open System.IO
open System.Text

type TreeEntry =
    {
        Mode : int
        Name : string
        Hash : Hash
    }

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
        let consumeTo (stopAt : byte) : byte array option =
            let rec consumeTo () : byte seq =
                seq {
                    let b = b.ReadByte ()
                    if b < 0 then failwithf "Stream ended in the middle while consuming to '%i'." stopAt
                    if b <> int stopAt then
                        yield byte b
                        yield! consumeTo ()
                }

            // Read the first one to see if we can
            let firstByte = b.ReadByte ()
            if firstByte < 0 then None else
            seq {
                yield byte firstByte
                yield! consumeTo ()
            }
            |> Seq.toArray
            |> Some

        let consume (n : int) : byte array =
            let output = Array.zeroCreate<byte> n
            let total = b.Read (output, 0, n)
            if total <> n then failwithf "Reached the end of the stream while consuming %i bytes" n
            output

        let stripRow () : TreeEntry option =
            let mode = consumeTo 32uy
            match mode with
            | None -> None
            | Some mode ->
            let name = consumeTo 0uy
            match name with
            | None -> failwith "Stream ended before we could consume a name"
            | Some name ->
            let hash = consume 20
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
