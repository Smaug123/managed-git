namespace Git.Internals

open Git

open System.IO
open System.Text
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

type OneOf = OneOf of string list

[<RequireQualifiedAccess>]
module Parse =

    let private parseInt (chars : byte array) =
        let rec acc (i : int) (soFar : int) =
            if i = chars.Length then
                soFar
            else if byte '0' <= chars.[i] && chars.[i] <= byte '9' then
                acc (i + 1) (10 * soFar + int (chars.[i] - byte '0'))
            else
                failwithf "non-digit character '%i' ('%c') at index %i" chars.[i] (char chars.[i]) i

        acc 0 0

    let consumeWord (context : string) (OneOf expecting) (s : Stream) =
        let word = Stream.consumeTo s 32uy

        match word with
        | None ->
            failwithf
                "Expected a word '%s' in a %s object, but stream ran out"
                context
                (expecting |> String.concat "//")
        | Some word ->

        let word = word |> Encoding.UTF8.GetString

        if not <| List.contains word expecting then
            failwithf "Expected a word '%s' in a %s object, but got '%s'" (expecting |> String.concat "//") context word

        word

    let consumeHash (context : string) (s : Stream) =
        let hash = Stream.consumeTo s 10uy

        match hash with
        | None -> failwithf "Stream ended before we could read hash in context '%s'." context
        | Some hash -> hash |> Hash.ofSpelling

    let consumeLabelledHash (context : string) (expecting : OneOf) (s : Stream) : string * Hash =
        let w = consumeWord context expecting s
        let h = consumeHash (sprintf "%s: %s" context w) s
        w, h

    let consumePerson (id : string) (s : Stream) =
        let name =
            Stream.consumeTo s (byte '<')
            |> Option.map Encoding.UTF8.GetString

        match name with
        | None -> failwithf "No %s name present in object." id
        | Some name ->

        if name.[name.Length - 1] <> ' ' then
            failwithf "Name of %s '%s' unexpectedly fails to end with a space" id name

        let name = name.Substring (0, name.Length - 1)

        let email =
            Stream.consumeTo s (byte '>')
            |> Option.map Encoding.UTF8.GetString

        match email with
        | None -> failwithf "No %s email present in object." id
        | Some email ->

        let space = Stream.consumeTo s 32uy

        match space with
        | None -> failwithf "Object ended after %s email" id
        | Some s ->
            if s.Length <> 0 then
                failwithf "Expected a space immediately after %s email, got '%s'" id (Encoding.UTF32.GetString s)

        let timestamp = Stream.consumeTo s 32uy

        match timestamp with
        | None -> failwithf "Commit object ended before %s timestamp" id
        | Some timestamp ->

        let timestamp = parseInt timestamp * 1<second>

        let offset =
            Stream.consumeTo s 10uy
            |> Option.map Encoding.UTF8.GetString

        match offset with
        | None -> failwithf "Commit object ended before %s timezone" id
        | Some offset ->

        {
            Name = name
            Email = email
            Date = timestamp
            DateTimezone = offset
        }
