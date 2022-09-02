namespace Git

open System.IO
open System.IO.Compression
open System.Security.Cryptography

type EncodedObject =
    {
        Header : Header
        Content : byte array
    }

[<RequireQualifiedAccess>]
module EncodedObject =
    let encode (o : Git.Object) : EncodedObject =
        let contents =
            match o with
            | Object.Blob c -> Blob.encode c
            | Object.Tree entries -> Tree.encode entries
            | Object.Commit c -> Commit.encode c

        {
            Header =
                match o with
                | Object.Blob _ -> Header.Blob contents.Length
                | Object.Tree _ -> Header.Tree contents.Length
                | Object.Commit _ -> Header.Commit contents.Length
            Content = contents
        }

    let decode (e : EncodedObject) : Git.Object =
        match e.Header with
        | Header.Tree _ -> Tree.decode e.Content |> Object.Tree
        | Header.Blob _ -> Blob.decode e.Content |> Object.Blob
        | Header.Commit _ -> Commit.decode e.Content |> Object.Commit

    let hash (o : EncodedObject) : Hash =
        use hasher = SHA1.Create ()
        let content = Array.concat [| Header.toBytes o.Header ; o.Content |]

        hasher.ComputeHash content |> Hash.ofBytes

    let private compress (o : EncodedObject) (dest : Stream) : unit =
        let toWrite =
            [| Header.toBytes o.Header ; o.Content |]
            |> Array.concat

        use ms = new MemoryStream (toWrite)
        use ds = new DeflateStream (dest, CompressionMode.Compress)
        ms.CopyTo ds

    /// Read the header of the stream seeked to the beginning of the content.
    let private consumeHeader (s : BinaryReader) : Header =
        let rec bytes () : byte seq =
            seq {
                let newByte = s.Read ()

                if newByte < 0 then
                    failwith "ran out of bytes"
                elif newByte > 0 then
                    yield (byte newByte)
                    yield! bytes ()
            // stop reading the header at the 0 byte
            }

        match bytes () |> Seq.toArray |> Header.ofAsciiBytes with
        | None -> failwith "malformed header"
        | Some b -> b

    let private uncompress (s : Stream) : EncodedObject =
        use ms = new MemoryStream ()
        use ds = new DeflateStream (s, CompressionMode.Decompress)
        ds.CopyTo ms
        ms.Seek (0L, SeekOrigin.Begin) |> ignore

        use r = new BinaryReader (ms)
        let header = consumeHeader r

        let expectedLength =
            match header with
            | Header.Blob i -> i
            | Header.Tree i -> i
            | Header.Commit i -> i

        let result =
            {
                Header = header
                Content = r.ReadBytes expectedLength
            }

        if r.PeekChar () <> -1 then
            failwith "unexpectedly not at end"

        result

    let write (r : Repository) (o : EncodedObject) : Hash =
        let hash = hash o
        let hashStr = Hash.toString hash
        let objectName = hashStr.[2..]
        let subDir = hashStr.[0..1]

        let d = Repository.createSubdir (Repository.objectDir r) subDir
        use filestream = r.Fs.File.Create (r.Fs.Path.Combine (d.FullName, objectName))

        compress o filestream

        hash

    let catFile (r : Repository) (hash : Hash) : EncodedObject =
        let hash = hash |> Hash.toString
        let objectName = hash.[2..]
        let subDir = hash.[0..1]

        use filestream =
            r.Fs.Path.Combine ((Repository.objectDir r).FullName, subDir, objectName)
            |> r.Fs.File.OpenRead

        uncompress filestream
