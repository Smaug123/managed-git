namespace Git

open System.IO
open System.Security.Cryptography
open System.IO.Compression

type Object =
    {
        Header : Header
        Content : byte array
    }

[<RequireQualifiedAccess>]
module Object =
    let hash (o : Object) : Hash =
        use hasher = SHA1.Create ()
        let content = Array.concat [| Header.toBytes o.Header ; o.Content |]

        hasher.ComputeHash content
        |> Hash.ofBytes

    let private compress (o : Object) (dest : Stream) : unit =
        let toWrite =
            [| Header.toBytes o.Header ; o.Content |]
            |> Array.concat

        use ms = new MemoryStream(toWrite)
        use ds = new DeflateStream(dest, CompressionMode.Compress)
        ms.CopyTo ds

    /// Read the header of the stream seeked to the beginning of the content.
    let private consumeHeader (s : BinaryReader) : Header =
        let rec bytes () : byte seq =
            seq {
                let newByte = s.Read ()
                if newByte < 0 then failwith "ran out of bytes"
                elif newByte > 0 then
                    yield (byte newByte)
                    yield! bytes ()
                // stop reading the header at the 0 byte
            }

        match bytes () |> Seq.toArray |> Header.ofBytes with
        | None ->
            failwith "malformed header"
        | Some b -> b

    let private uncompress (s : Stream) : Object =
        use ms = new MemoryStream ()
        use ds = new DeflateStream(s, CompressionMode.Decompress)
        ds.CopyTo ms
        ms.Seek(0L, SeekOrigin.Begin) |> ignore

        use r = new BinaryReader(ms)
        let header = consumeHeader r
        let expectedLength =
            match header with
            | Header.Blob i -> i
        let result =
            {
                Header = header
                Content = r.ReadBytes expectedLength
            }
        if r.PeekChar () <> -1 then failwith "unexpectedly not at end"
        result

    let write (r : Repository) (o : Object) : unit =
        let hash = hash o |> Hash.toString
        let objectName = hash.[2..]
        let subdir = hash.[0..1]

        let d = Repository.createSubdir (Repository.objectDir r) subdir
        use filestream = r.Fs.File.Create (r.Fs.Path.Combine (d.FullName, objectName))

        compress o filestream

    let catFile (r : Repository) (hash : Hash) : Object =
        let hash = hash |> Hash.toString
        let objectName = hash.[2..]
        let subdir = hash.[0..1]

        use filestream =
            r.Fs.Path.Combine ((Repository.objectDir r).FullName, subdir, objectName)
            |> r.Fs.File.OpenRead

        uncompress filestream
