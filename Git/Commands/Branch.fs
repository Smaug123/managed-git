namespace Git.Commands

open Git

[<RequireQualifiedAccess>]
module Branch =

    let create (r : Repository) (name : string) =
        // TODO: probably want to type this more strongly, do some more parsing of the target
        let (SymbolicRefTarget currentHead) =
            match SymbolicReference.lookup r SymbolicRef.Head with
            | Error e -> failwithf "Could not determine head to branch from: %s" (string<SymbolicRefLookupError> e)
            | Ok target -> target

        // Match Git's behaviour here!
        if not (currentHead.StartsWith "refs/heads/") then
            failwith "fatal: HEAD not found below refs/heads!"

        RevParse.parse r currentHead
        |> Reference.write r name
