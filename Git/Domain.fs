namespace Git

type BranchName =
    | BranchName of string

    /// Human-readable round-tripping representation.
    override this.ToString () =
        match this with
        | BranchName s -> s
