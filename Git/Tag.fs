namespace Git

open System.IO
open System.Text
open Git.Internals

type TaggedObjectType =
    | Commit
    override this.ToString () =
        match this with
        | TaggedObjectType.Commit ->
            "commit"

    static member Parse (s : string) =
        match s with
        | "commit" -> TaggedObjectType.Commit
        | _ -> failwithf "Unrecognised tagged object type: %s" s

type TagEntry =
    {
        Object : Hash
        Type : TaggedObjectType
        Name : string
        Tagger : Contributor
        Message : string
    }
    override this.ToString () =
        sprintf
            "object %O\ntype %O\ntag %s\ntagger %O\n\n%s"
            this.Object
            this.Type
            this.Name
            this.Tagger
            this.Message


[<RequireQualifiedAccess>]
module Tag =
    let encode (entry : TagEntry) : byte array =
        entry.ToString ()
        |> Encoding.UTF8.GetBytes

    let decode (file : byte array) : TagEntry =
        use ms = new MemoryStream (file)

        let objectHash =
            Parse.consumeWord "tag" (OneOf ["object"]) ms |> ignore

            match Stream.consumeTo ms (byte '\n') with
            | None -> failwith "Tag object should have had a newline in"
            | Some h -> h |> Hash.ofSpelling

        let typeReferredTo =
            Parse.consumeWord "tag" (OneOf ["type"]) ms |> ignore

            match Stream.consumeTo ms (byte '\n') with
            | None -> failwith "Tag type should have had a newline in"
            | Some h -> h |> Encoding.ASCII.GetString |> TaggedObjectType.Parse

        let tagName =
            Parse.consumeWord "tag" (OneOf ["tag"]) ms |> ignore

            match Stream.consumeTo ms (byte '\n') with
            | None -> failwith "Tag name should have had a newline in"
            | Some t -> t |> Encoding.ASCII.GetString

        let tagger =
            Parse.consumeWord "tag" (OneOf ["tagger"]) ms |> ignore
            Parse.consumePerson "tagger" ms

        let trailingNewline = Stream.consumeTo ms 10uy
        match trailingNewline with
        | None -> failwith "Tag unexpectedly ended before message"
        | Some [| |] -> ()
        | Some arr ->
            failwithf "Unexpectedly received data in between tagger and message: %+A" arr

        let message = Stream.consumeToEnd ms |> Encoding.UTF8.GetString

        {
            Object = objectHash
            Type = typeReferredTo
            Name = tagName
            Tagger = tagger
            Message = message
        }