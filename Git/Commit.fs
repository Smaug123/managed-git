namespace Git

open System.IO
open System.Text
open Git.Internals

type CommitEntry =
    {
        Tree : Hash
        Parents : Hash list
        Committer : Contributor
        GpgSignature : string option
        Author : Contributor
        CommitMessage : string
    }

    override this.ToString () =
        sprintf
            "tree %O\n%sauthor %O\ncommitter %O\n\n%s"
            this.Tree
            (this.Parents
             |> List.map (Hash.toString >> sprintf "parent %s\n")
             |> String.concat "\n")
            this.Author
            this.Committer
            this.CommitMessage

[<RequireQualifiedAccess>]
module Commit =

    let private assertValid (context : string) (s : string) : unit =
        if s.IndexOfAny [| '<' ; '\n' |] > 0 then
            failwithf "%s '%s' contains forbidden character" context s

    let encode (content : CommitEntry) : byte array =
        assertValid "Author name" content.Author.Name
        assertValid "Committer name" content.Committer.Name
        assertValid "Author email" content.Author.Email
        assertValid "Committer email" content.Committer.Email

        seq {
            yield sprintf "tree %s" (Hash.toString content.Tree)

            yield!
                content.Parents
                |> List.map (Hash.toString >> sprintf "parent %s")
                |> Array.ofList

            yield
                sprintf
                    "author %s <%s> %i %s"
                    content.Author.Name
                    content.Author.Email
                    content.Author.Date
                    content.Author.DateTimezone

            yield
                sprintf
                    "committer %s <%s> %i %s"
                    content.Committer.Name
                    content.Committer.Email
                    content.Committer.Date
                    content.Committer.DateTimezone

            yield sprintf "\n%s" content.CommitMessage
        }
        |> String.concat "\n"
        // TODO: assumption that may not be compatible with Git: UTF8 is used for names, emails etc
        |> Encoding.UTF8.GetBytes

    let decode (file : byte array) : CommitEntry =
        use ms = new MemoryStream (file)

        let treeWord, treeHash = Parse.consumeLabelledHash "commit" (OneOf [ "tree" ]) ms

        if treeWord <> "tree" then
            failwithf "Malformed tree indicator '%s'" treeWord

        let parents, author =
            let rec consumeParentsAndAuthor (parents : Hash list) =
                let w = Parse.consumeWord "commit" (OneOf [ "author" ; "parent" ]) ms

                if w = "parent" then
                    let parent = Parse.consumeHash "parent" ms
                    consumeParentsAndAuthor (parent :: parents)
                elif w = "author" then
                    let author = Parse.consumePerson "author" ms
                    parents, author
                else
                    failwithf "Expected author or parent, got '%s'" w

            consumeParentsAndAuthor []

        let _ = Parse.consumeWord "commit" (OneOf [ "committer" ]) ms
        let committer = Parse.consumePerson "committer" ms

        let trailingNewline = Stream.consumeTo ms 10uy

        let gpgSignature =
            match trailingNewline with
            | None -> failwith "Commit object ended at end of committer"
            | Some [||] -> None
            | Some data when data.[0..6] = Encoding.ASCII.GetBytes "gpgsig " ->
                let result = StringBuilder ()

                result.Append (Encoding.ASCII.GetString data.[7..])
                |> ignore

                result.Append '-' |> ignore

                let remaining =
                    match Stream.consumeTo ms (byte '-') with
                    | None -> failwith "GPG signature unexpectedly did not terminate in '-' character"
                    | Some s -> Encoding.ASCII.GetString s

                result.Append remaining |> ignore

                let trailer =
                    match Stream.consumeTo ms 10uy with
                    | None -> failwith "GPG signature was not followed by a newline"
                    | Some s -> Encoding.ASCII.GetString s

                result.Append trailer |> ignore
                result.ToString () |> Some
            | Some data -> failwithf "Unexpected trailer to committer, got %s" (Encoding.UTF8.GetString data)

        let message = Stream.consumeToEnd ms |> Encoding.UTF8.GetString

        {
            Committer = committer
            Author = author
            CommitMessage = message
            GpgSignature = gpgSignature
            Tree = treeHash
            Parents = parents
        }
