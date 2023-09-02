namespace Git

open System

[<RequireQualifiedAccess>]
module internal String =

    let chopStart (toChop : string) (s : string) =
        if s.StartsWith (toChop, StringComparison.Ordinal) then
            s.Substring toChop.Length
        else
            s

    let chopEnd (toChop : string) (s : string) =
        if s.EndsWith (toChop, StringComparison.Ordinal) then
            s.Substring (0, s.Length - toChop.Length)
        else
            s
