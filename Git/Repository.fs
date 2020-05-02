namespace Git

open System.IO.Abstractions

/// If you have a Repository in scope, you know it existed at the time it was created.
type Repository =
    private
        {
            Directory : IDirectoryInfo
        }
    member this.Fs = this.Directory.FileSystem

type InitFailure =
    | DirectoryDoesNotExist
    | AlreadyGit

[<RequireQualifiedAccess>]
module Repository =
    let internal gitDir (r : Repository) : IDirectoryInfo =
        r.Fs.Path.Combine(r.Directory.FullName, ".git") |> r.Fs.DirectoryInfo.FromDirectoryName

    let internal objectDir (r : Repository) : IDirectoryInfo =
        r.Fs.Path.Combine((gitDir r).FullName, "objects") |> r.Fs.DirectoryInfo.FromDirectoryName

    let internal createSubdir (r : IDirectoryInfo) (name : string) : IDirectoryInfo =
        let output =
            r.FileSystem.Path.Combine(r.FullName, name)
            |> r.FileSystem.DirectoryInfo.FromDirectoryName
        output.Create ()
        output

    let init (dir : IDirectoryInfo) : Result<Repository, InitFailure> =
        if not dir.Exists then Error DirectoryDoesNotExist
        elif not <| Seq.isEmpty (dir.EnumerateDirectories ".git") then Error AlreadyGit
        else

        let r =
            {
                Directory = dir
            }

        let gitDir = createSubdir dir ".git"
        let objectDir = createSubdir gitDir "objects"
        let packDir = createSubdir objectDir "pack"
        let infoDir = createSubdir objectDir "info"

        r
        |> Ok

