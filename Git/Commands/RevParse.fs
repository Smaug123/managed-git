namespace Git.Commands

open Git

[<RequireQualifiedAccess>]
module RevParse =

    let rec parse (r : Repository) (s : string) : Hash =
        if s = "@" then
            parse r "HEAD"
        else

            match RevParse.disambiguateLoose r s with
            | [ s ] -> s
            | _ :: _ :: _ -> failwithf "fatal: ambiguous argument '%s'" s
            | [] ->

            failwith ""
