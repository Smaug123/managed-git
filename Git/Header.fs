namespace Git

open System

type Header =
    | Blob of int // length of content
    | Tree of int // length of content
    | Commit of int // length of content
// | Tag

[<RequireQualifiedAccess>]
module internal Header =

    let toBytes (h : Header) : byte array =
        let s =
            match h with
            | Header.Blob length -> sprintf "blob %i" length
            | Header.Tree length -> sprintf "tree %i" length
            | Header.Commit length -> sprintf "commit %i" length

        // If perf critical, could optimise allocation here
        Array.append (System.Text.Encoding.ASCII.GetBytes s) [| 0uy |]

    let ofAsciiBytes (s : byte array) : Header option =
        if s.Length <= 5 then
            None
        else
            match s.[0] with
            | 98uy ->
                // 'b', then "lob"
                if s.[1] = 108uy && s.[2] = 111uy && s.[3] = 98uy then
                    let number = s.[5..] |> Array.map char |> String |> Int32.Parse
                    Header.Blob number |> Some
                else
                    None
            | 116uy ->
                // 't', then "ree"
                if s.[1] = 114uy && s.[2] = 101uy && s.[3] = 101uy then
                    let number = s.[5..] |> Array.map char |> String |> Int32.Parse
                    Header.Tree number |> Some
                else
                    None
            | 99uy ->
                // 'c', then "ommit"
                if
                    s.Length > 7
                    && s.[1] = 111uy
                    && s.[2] = 109uy
                    && s.[3] = 109uy
                    && s.[4] = 105uy
                    && s.[5] = 116uy
                then
                    let number = s.[7..] |> Array.map char |> String |> Int32.Parse
                    Header.Commit number |> Some
                else
                    None
            | _ -> None
