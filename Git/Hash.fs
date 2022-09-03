namespace Git

open System
open System.Globalization
open System.Text

type Hash =
    private
    | Hash of byte array

    override this.ToString () =
        match this with
        | Hash h ->

        let t = StringBuilder (h.Length * 2)

        for b in h do
            t.AppendFormat ("{0:x2}", b) |> ignore

        t.ToString ()

[<RequireQualifiedAccess>]
module Hash =

    let ofBytes (s : byte array) = s |> Hash

    let ofString (s : string) : Hash =
        let rec b (pos : int) =
            seq {
                if pos < s.Length then
                    yield Byte.Parse (s.Substring (pos, 2), NumberStyles.AllowHexSpecifier)
                    yield! b (pos + 2)
            }

        b 0 |> Seq.toArray |> ofBytes

    // Given a byte array of *characters* spelling out e.g. 'b' 'd' '6' '3', return the hash this is spelling out.
    let ofSpelling (input : byte array) : Hash =
        let inline value (b : byte) =
            let c = char b

            if '0' <= c && c <= '9' then
                b - byte '0'
            elif 'A' <= c && c <= 'F' then
                b - (byte 'A') + 10uy
            elif 'a' <= c && c <= 'f' then
                b - (byte 'a') + 10uy
            else
                failwithf "Byte '%i' ('%c') is not a hex digit" b (char b)

        let rec b (pos : int) =
            seq {
                if pos < input.Length then
                    yield value input.[pos] * 16uy + value input.[pos + 1]
                    yield! b (pos + 2)
            }

        fun i ->
            value input.[2 * i] * 16uy
            + value input.[2 * i + 1]
        |> Array.init (input.Length / 2)
        |> ofBytes

    let toString (h : Hash) : string = h.ToString ()
