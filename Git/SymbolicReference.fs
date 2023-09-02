namespace Git

open System
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

type SymbolicRefWriteError =
    | PointingOutsideRefs of SymbolicRef

    override this.ToString () =
        match this with
        | SymbolicRefWriteError.PointingOutsideRefs ref -> sprintf "refusing to point %O outside of refs/" ref

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
            if not (contents.StartsWith ("ref: ", StringComparison.Ordinal)) then
                Error (MalformedRef (name, contents))
            elif not (contents.EndsWith ("\n", StringComparison.Ordinal)) then
                Error (MalformedRef (name, contents))
            else
                // Omit the trailing newline
                contents.Substring (5, contents.Length - 6) |> SymbolicRefTarget |> Ok
        )

    let write (r : Repository) (name : SymbolicRef) (contents : string) : Result<unit, SymbolicRefWriteError> =
        if not <| contents.StartsWith ("refs/", StringComparison.Ordinal) then
            Error (SymbolicRefWriteError.PointingOutsideRefs name)

        else

        r.Fs.File.WriteAllText ((SymbolicRef.getFile r name).FullName, sprintf "ref: %s\n" contents)
        Ok ()

    let delete (r : Repository) (name : SymbolicRef) : unit =
        let underlyingFile = SymbolicRef.getFile r name
        underlyingFile.Delete ()
