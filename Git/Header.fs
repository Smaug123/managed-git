namespace Git

/// An object type and the length of its content.
type internal Header = ObjectType * int

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
            | ObjectType.Blob, length -> sprintf "blob %i" length
            | ObjectType.Tree, length -> sprintf "tree %i" length
            | ObjectType.Commit, length -> sprintf "commit %i" length
            | ObjectType.Tag, length ->
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
                    (ObjectType.Blob, number) |> Some
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
                    (ObjectType.Tree, number) |> Some
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
                    (ObjectType.Commit, number) |> Some
                else
                    None
            | _ -> None
