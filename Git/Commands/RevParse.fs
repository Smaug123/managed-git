namespace Git.Commands

open Git

type RevParseError =
    | MultipleMatches of original : string * candidates : Hash list
    | Unrecognised of original : string

    override this.ToString () =
        match this with
        | RevParseError.MultipleMatches (s, candidates) -> sprintf "fatal: ambiguous argument '%s'" s
        | RevParseError.Unrecognised s ->
            sprintf "fatal: ambiguous argument '%s': unknown revision or path not in the working tree." s

[<RequireQualifiedAccess>]
module RevParse =

    let rec parse (r : Repository) (s : string) : Result<Hash, RevParseError> =
        if s = "@" then
            parse r "HEAD"
        else

        match RevParse.disambiguateLoose r s with
        | [ s ] -> Ok s
        | (_ :: _ :: _) as matches -> Error (RevParseError.MultipleMatches (s, matches))
        | [] -> Error (RevParseError.Unrecognised s)
