namespace Git

open System.Text

[<Struct>]
type Hash = Hash of byte list

[<RequireQualifiedAccess>]
module Hash =

    let ofBytes s = s |> Seq.toList |> Hash

    let toString (Hash h) : string =
        let t = StringBuilder (List.length h * 2)
        h
        |> List.iter (fun b -> t.AppendFormat ("{0:x2}" , b) |> ignore)
        t.ToString ()
