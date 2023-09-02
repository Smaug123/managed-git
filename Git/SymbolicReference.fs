namespace Git

open System.IO
open System.IO.Abstractions

/// The target of a symbolic reference, e.g. "refs/heads/blah".
type SymbolicRefTarget = | SymbolicRefTarget of string

type SymbolicRef =
    | CherryPickHead
    | RevertHead
    | FetchHead
    | MergeHead
    | Head
    | OrigHead
    // TODO - determine how an arbitrary symbolic ref actually behaves
    | Verbatim of string

    override this.ToString () : string =
        match this with
        | CherryPickHead -> "CHERRY_PICK_HEAD"
        | RevertHead -> "REVERT_HEAD"
        | FetchHead -> "FETCH_HEAD"
        | MergeHead -> "MERGE_HEAD"
        | Head -> "HEAD"
        | OrigHead -> "ORIG_HEAD"
        | Verbatim s -> s

[<RequireQualifiedAccess>]
module SymbolicRef =
    let getFile (r : Repository) (name : SymbolicRef) : IFileInfo =
        name.ToString ()
        |> fun i ->
            r.Fs.Path.Combine ((Repository.gitDir r).FullName, i)
            |> r.Fs.FileInfo.FromFileName

type SymbolicRefLookupError =
    | RefDidNotExist of SymbolicRef
    | MalformedRef of SymbolicRef * string

    override this.ToString () =
        match this with
        | SymbolicRefLookupError.RefDidNotExist s -> sprintf "Symbolic ref %s did not exist" (string<SymbolicRef> s)
        | SymbolicRefLookupError.MalformedRef (ref, contents) ->
            sprintf "Symbolic ref %s had malformed contents: %s" (string<SymbolicRef> ref) contents

[<RequireQualifiedAccess>]
module SymbolicReference =

    /// This is effectively `git symbolic-ref NAME`.
    let lookup (r : Repository) (name : SymbolicRef) : Result<SymbolicRefTarget, SymbolicRefLookupError> =
        let f = SymbolicRef.getFile r name

        let text =
            try
                r.Fs.File.ReadAllText f.FullName |> Ok
            with :? FileNotFoundException ->
                Error (RefDidNotExist name)

        text
        |> Result.bind (fun contents ->
            if contents.Substring (0, 5) = "ref: " then
                contents.Substring 5 |> SymbolicRefTarget |> Ok
            else
                Error (MalformedRef (name, contents))
        )

    let write (r : Repository) (name : SymbolicRef) (contents : string) : unit =
        if not <| contents.StartsWith "refs/" then
            failwithf "refusing to point %O outside of refs/" name

        r.Fs.File.WriteAllText ((SymbolicRef.getFile r name).FullName, sprintf "ref: %s" contents)

    let delete (r : Repository) (name : SymbolicRef) : unit = (SymbolicRef.getFile r name).Delete ()
