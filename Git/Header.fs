namespace Git

open System

type Header =
    | Blob of int // length of content
    | Tree of int // length of content
    // | Commit
    // | Tag

[<RequireQualifiedAccess>]
module internal Header =

    let toBytes (h : Header) : byte array =
        let s =
            match h with
            | Header.Blob length ->
                // TODO - internationalisation issue here
                sprintf "blob %i" length
            | Header.Tree length ->
                sprintf "tree %i" length
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
        elif s.[0..3] = ("tree".ToCharArray () |> Array.map byte) then
            let number = s.[5..] |> Array.map char |> String |> Int32.Parse
            Header.Tree number
            |> Some
        else None
