namespace Git.Internals

open System.IO

[<RequireQualifiedAccess>]
module internal Stream =

    /// Consume the stream until and including the first instance of `stopAt`.
    /// Return None instead if the stream is already used up; throw if we hit the end of the stream
    /// before hitting `stopAt`.
    let consumeTo (b : Stream) (stopAt : byte) : byte array option =
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

        let firstByte = byte firstByte
        if firstByte = stopAt then Array.empty |> Some
        else
            seq {
                yield firstByte
                yield! consumeTo ()
            }
            |> Seq.toArray
            |> Some

    /// Consume the first n bytes of the stream. Throw if the stream runs out first.
    let consume (b : Stream) (n : int) : byte array =
        let output = Array.zeroCreate<byte> n
        let total = b.Read (output, 0, n)
        if total <> n then failwithf "Reached the end of the stream while consuming %i bytes" n
        output

    let consumeToEnd (b : MemoryStream)  : byte array =
        use newMs = new MemoryStream()
        b.CopyTo(newMs)
        newMs.ToArray ()
