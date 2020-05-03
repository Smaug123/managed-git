namespace Git

open System
open System.IO
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open Git.Internals

type Contributor =
    {
        Name : string
        Email : string
        Date : int<second>
        DateTimezone : string
    }
    override this.ToString () =
        sprintf "%s <%s> %i %s" this.Name this.Email this.Date this.DateTimezone

type CommitEntry =
    {
        Tree : Hash
        Parents : Hash list
        Committer : Contributor
        Author : Contributor
        CommitMessage : string
    }
    override this.ToString () =
        sprintf
            "tree %O\n%sauthor %O\ncommitter %O\n\n%s"
            this.Tree
            (this.Parents |> List.map (Hash.toString >> sprintf "parent %s\n") |> String.concat "\n")
            this.Author
            this.Committer
            this.CommitMessage

// TODO - implement signed commits too
[<RequireQualifiedAccess>]
module Commit =
    let encode (content : CommitEntry) : byte array =
        if content.Author.Name.Contains '<' || content.Author.Name.Contains '\n' then
            failwithf "Author name '%s' contains forbidden character" content.Author.Name
        if content.Committer.Name.Contains '<' || content.Committer.Name.Contains '\n' then
            failwithf "Committer name '%s' contains forbidden character" content.Committer.Name
        if content.Author.Email.Contains '>' || content.Author.Email.Contains '\n' then
            failwithf "Author email '%s' contains forbidden character" content.Author.Email
        if content.Committer.Email.Contains '>' || content.Committer.Email.Contains '\n' then
            failwithf "Committer email '%s' contains forbidden character" content.Committer.Email
        seq {
            yield sprintf "tree %s" (Hash.toString content.Tree)
            yield! content.Parents |> List.map (Hash.toString >> sprintf "parent %s") |> Array.ofList
            yield sprintf "author %s <%s> %i %s" content.Author.Name content.Author.Email content.Author.Date content.Author.DateTimezone
            yield sprintf "committer %s <%s> %i %s" content.Committer.Name content.Committer.Email content.Committer.Date content.Committer.DateTimezone
            yield sprintf "\n%s" content.CommitMessage
        }
        |> String.concat "\n"
        |> fun s -> s.ToCharArray ()
        |> Array.map byte

    let private parseInt (chars : byte array) =
        let rec acc (i : int) (soFar : int) =
            if i = chars.Length then soFar else
            if byte '0' <= chars.[i] && chars.[i] <= byte '9' then
                acc (i + 1) (10 * soFar + int (chars.[i] - byte '0'))
            else failwithf "non-digit character '%i' ('%c') at index %i" chars.[i] (char chars.[i]) i
        acc 0 0

    let decode (file : byte array) : CommitEntry =
        use ms = new MemoryStream(file)

        let consumeWord (OneOf expecting) =
            let word = Stream.consumeTo ms 32uy
            match word with
            | None ->
                failwithf "Expected a word '%s' in a commit object, but stream ran out" (expecting |> String.concat "//")
            | Some word ->
            let word =
                word
                |> Array.map char
                |> String
            if not <| List.contains word expecting then
                failwithf "Expected a word '%s' in a commit object, but got '%s'" (expecting |> String.concat "//") word
            word

        let consumeHash (context : string) =
            let hash = Stream.consumeTo ms 10uy
            match hash with
            | None -> failwithf "Stream ended before we could read hash in context '%s'." context
            | Some hash ->
            hash |> Hash.ofSpelling

        let consumeLabelledHash (expecting : OneOf) : string * Hash =
            let w = consumeWord (expecting)
            let h = consumeHash w
            w, h

        let consumePerson (id : string) =
            let name = Stream.consumeTo ms (byte '<') |> Option.map (Array.map char >> String)
            match name with
            | None ->
                failwithf "No %s name present in commit object." id
            | Some name ->
            if name.[name.Length - 1] <> ' ' then
                failwithf "Name of %s '%s' unexpectedly fails to end with a space" id name
            let name = name.Substring (0, name.Length - 1)

            let email = Stream.consumeTo ms (byte '>') |> Option.map (Array.map char >> String)
            match email with
            | None ->
                failwithf "No %s email present in commit object." id
            | Some email ->

            let space = Stream.consumeTo ms 32uy
            match space with
            | None -> failwithf "Commit object ended after %s email" id
            | Some s -> if s.Length <> 0 then failwithf "Expected a space immediately after %s email, got '%s'" id (s |> Array.map char |> String)

            let timestamp = Stream.consumeTo ms 32uy
            match timestamp with
            | None -> failwithf "Commit object ended before %s timestamp" id
            | Some timestamp ->
            let timestamp = parseInt timestamp * 1<second>

            let offset = Stream.consumeTo ms 10uy |> Option.map (Array.map char >> String)
            match offset with
            | None -> failwithf "Commit object ended before %s timezone" id
            | Some offset ->

            {
                Name = name
                Email = email
                Date = timestamp
                DateTimezone = offset
            }

        let treeWord, treeHash = consumeLabelledHash (OneOf ["tree"])
        if treeWord <> "tree" then
            failwithf "Malformed tree indicator '%s'" treeWord

        let parents, author =
            let rec consumeParentsAndAuthor (parents : Hash list) =
                let w = consumeWord (OneOf ["author" ; "parent" ])
                if w = "parent" then
                    let parent = consumeHash "parent"
                    consumeParentsAndAuthor (parent :: parents)
                elif w = "author" then
                    let author = consumePerson "author"
                    parents, author
                else
                    failwithf "Expected author or parent, got '%s'" w

            consumeParentsAndAuthor []

        let _ = consumeWord (OneOf ["committer"])
        let committer = consumePerson "committer"

        let trailingNewline = Stream.consumeTo ms 10uy
        match trailingNewline with
        | None -> failwith "Commit object ended at end of committer"
        | Some s -> if s.Length <> 0 then failwithf "Expected an extra newline immediately after committer, got %s" (s |> Array.map char |> String)

        let message = Stream.consumeToEnd ms |> Array.map char |> String
        //if message.[message.Length - 1] <> '\n' then
        //    failwithf "Expected commit message to end with newline, got '%c':\n%s" message.[message.Length - 1] message
        //let message = message.Substring(0, message.Length - 1)

        {
            Committer = committer
            Author = author
            CommitMessage = message
            Tree = treeHash
            Parents = parents
        }
