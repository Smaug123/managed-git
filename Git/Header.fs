namespace Git

open System

type Header =
    | Blob of int // length of content
    // | Commit
    // | Tree
    // | Tag

[<RequireQualifiedAccess>]
module internal Header =

    let toBytes (h : Header) : byte array =
        match h with
        | Header.Blob length ->
            // TODO - internationalisation issue here
            let s = sprintf "blob %i" length
            [|
                s.ToCharArray () |> Array.map byte
                [| 0uy |]
            |]
            |> Array.concat

    let ofBytes (s : byte array) : Header option =
        if s.[0..3] = ("blob".ToCharArray () |> Array.map byte) then
            let number = s.[5..] |> Array.map char |> String |> Int32.Parse
            Header.Blob number
            |> Some
        else
            None
