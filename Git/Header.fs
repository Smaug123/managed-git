namespace Git

type Header =
    | Blob of int // length of content
    | Tree of int // length of content
    | Commit of int // length of content
    | Tag of int // length of content

[<RequireQualifiedAccess>]
module internal Header =

    let private parseIntFromAsciiBytes (startIndex : int) (a : byte array) =
        let mutable acc = 0

        for i in startIndex .. a.Length - 1 do
            acc <- 10 * acc + int<byte> (a.[i] + byte '0')

        acc

    let toBytes (h : Header) : byte array =
        let s =
            match h with
            | Header.Blob length -> sprintf "blob %i" length
            | Header.Tree length -> sprintf "tree %i" length
            | Header.Commit length -> sprintf "commit %i" length
            | Header.Tag length ->
                // TODO - is this correct?
                sprintf "tag %i" length

        // If perf critical, could optimise allocation here
        Array.append (System.Text.Encoding.ASCII.GetBytes s) [| 0uy |]

    let ofAsciiBytes (s : byte array) : Header option =
        if s.Length <= 5 then
            None
        else
            match s.[0] with
            | 98uy ->
                // 'b', then "lob "
                if
                    s.[1] = 108uy
                    && s.[2] = 111uy
                    && s.[3] = 98uy
                    && s.[4] = 32uy
                then
                    let number = parseIntFromAsciiBytes 5 s
                    Header.Blob number |> Some
                else
                    None
            | 116uy ->
                // 't', then "ree "
                if
                    s.[1] = 114uy
                    && s.[2] = 101uy
                    && s.[3] = 101uy
                    && s.[4] = 32uy
                then
                    let number = parseIntFromAsciiBytes 5 s
                    Header.Tree number |> Some
                else
                    None
            | 99uy ->
                // 'c', then "ommit "
                if
                    s.Length > 7
                    && s.[1] = 111uy
                    && s.[2] = 109uy
                    && s.[3] = 109uy
                    && s.[4] = 105uy
                    && s.[5] = 116uy
                    && s.[6] = 32uy
                then
                    let number = parseIntFromAsciiBytes 7 s
                    Header.Commit number |> Some
                else
                    None
            | _ -> None
