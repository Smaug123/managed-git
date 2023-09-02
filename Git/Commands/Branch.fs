namespace Git.Commands

open Git

type BranchCreationError =
    | HeadNotBelowRefsHeads of currentHead : string
    | HeadDoesNotExist of SymbolicRefLookupError

    override this.ToString () =
        match this with
        | BranchCreationError.HeadNotBelowRefsHeads _ -> "fatal: HEAD not found below refs/heads!"
        | HeadDoesNotExist (err : SymbolicRefLookupError) ->
            // TODO: determine what Git does here
            sprintf "Could not determine head to branch from: %O" err

[<RequireQualifiedAccess>]
module Branch =

    let create (r : Repository) (name : string) (baseRef : string) =
        match Commands.RevParse.parse r baseRef with
        | Ok ref -> Reference.write r name ref |> Ok
        | Error (e : RevParseError) ->
            // TODO: find out what Git does here
            failwithf "Supplied ref is not known: %O" e

    let createFromHead (r : Repository) (name : string) : Result<ReferenceUpdate, BranchCreationError> =
        // TODO: probably want to type this more strongly, do some more parsing of the target
        match SymbolicReference.lookup r SymbolicRef.Head with
        | Error e -> Error (BranchCreationError.HeadDoesNotExist e)
        | Ok (SymbolicRefTarget currentHead) ->

        // Match Git's behaviour here!
        if not (currentHead.StartsWith "refs/heads/") then
            Error (BranchCreationError.HeadNotBelowRefsHeads currentHead)
        else
            create r name currentHead
