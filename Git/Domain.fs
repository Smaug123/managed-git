namespace Git

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

type BranchName =
    | BranchName of string

    /// Human-readable round-tripping representation.
    override this.ToString () =
        match this with
        | BranchName s -> s

type Contributor =
    {
        Name : string
        Email : string
        Date : int<second>
        DateTimezone : string
    }

    override this.ToString () =
        sprintf "%s <%s> %i %s" this.Name this.Email this.Date this.DateTimezone

type ObjectType =
    | Commit
    | Blob
    | Tag
    | Tree

    override this.ToString () =
        match this with
        | ObjectType.Commit -> "commit"
        | ObjectType.Blob -> "blob"
        | ObjectType.Tag -> "tag"
        | ObjectType.Tree -> "tree"
