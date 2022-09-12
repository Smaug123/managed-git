namespace Git.Commands

open Git

[<RequireQualifiedAccess>]
module VerifyPack =

    let verifyVerbose (printer : Printer) (repo : Repository) (input : string) =
        // TODO: this is not an exact match with Git, in that `git verify-pack`
        // specifies a path to a file, not a hash's integrity to verify.
        let verification = VerifyPack.verify repo (Hash.ofString input)
        printer.WriteLine (string<PackVerification> verification)
