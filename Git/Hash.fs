namespace Git

open System
open System.Globalization
open System.Text

[<Struct>]
type Hash = Hash of byte list

[<RequireQualifiedAccess>]
module Hash =

    let ofBytes s = s |> Seq.toList |> Hash
    let ofString (s : string) : Hash =
        let rec b (pos : int) =
            seq {
                if pos < s.Length then
                    yield Byte.Parse (s.Substring (pos, 2), NumberStyles.AllowHexSpecifier)
                    yield! b (pos + 2)
            }
        b 0
        |> ofBytes

    let toString (Hash h) : string =
        let t = StringBuilder (List.length h * 2)
        h
        |> List.iter (fun b -> t.AppendFormat ("{0:x2}" , b) |> ignore)
        t.ToString ()
