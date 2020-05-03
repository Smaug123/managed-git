namespace Git.Commands

open Git

[<RequireQualifiedAccess>]
module Log =

    let log (repo : Repository) (h : Hash) : Map<Hash, CommitEntry> =
        let rec log (h : Hash) (c : CommitEntry) : seq<Hash * CommitEntry> =
            seq {
                yield (h, c)
                yield!
                    c.Parents
                    |> List.map (fun i ->
                        match EncodedObject.catFile repo i |> EncodedObject.decode with
                        | Object.Commit c -> (i, c)
                        | s -> failwithf "Not a commit: %O (%+A)" i s)
                    |> Seq.collect (fun (i, c) -> log i c)
            }

        h
        |> EncodedObject.catFile repo
        |> EncodedObject.decode
        |> function | Object.Commit h -> h | s -> failwithf "Not a commit: %+A" s
        |> log h
        |> Map.ofSeq
