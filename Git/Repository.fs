namespace Git

open System.IO.Abstractions

/// If you have a Repository in scope, you know it existed at the time it was created.
type Repository =
    private
        {
            Directory : IDirectoryInfo
            IsCaseSensitive : bool
        }

    member this.Fs = this.Directory.FileSystem

type InitFailure =
    | DirectoryDoesNotExist
    | AlreadyGit

[<RequireQualifiedAccess>]
module Repository =
    let gitDir (r : Repository) : IDirectoryInfo =
        r.Fs.Path.Combine (r.Directory.FullName, ".git")
        |> r.Fs.DirectoryInfo.FromDirectoryName

    let internal objectDir (r : Repository) : IDirectoryInfo =
        r.Fs.Path.Combine ((gitDir r).FullName, "objects")
        |> r.Fs.DirectoryInfo.FromDirectoryName

    let internal refDir (r : Repository) : IDirectoryInfo =
        r.Fs.Path.Combine ((gitDir r).FullName, "refs")
        |> r.Fs.DirectoryInfo.FromDirectoryName

    let internal createSubdir (r : IDirectoryInfo) (name : string) : IDirectoryInfo =
        let output =
            r.FileSystem.Path.Combine (r.FullName, name)
            |> r.FileSystem.DirectoryInfo.FromDirectoryName

        output.Create ()
        output

    let make (dir : IDirectoryInfo) : Repository option =
        let fs = dir.FileSystem
        let gitPath = fs.Path.Combine (dir.FullName, ".git")

        if dir.Exists && fs.Directory.Exists gitPath then
            {
                Directory = dir
                IsCaseSensitive = not (fs.Directory.Exists (gitPath.ToUpperInvariant ()))
            }
            |> Some
        else
            None

    let init (dir : IDirectoryInfo) : Result<Repository, InitFailure> =
        match make dir with
        | Some _ -> Error AlreadyGit
        | None ->

        if not dir.Exists then
            Error DirectoryDoesNotExist
        else

        // TODO do this atomically
        let gitDir = createSubdir dir ".git"
        let objectDir = createSubdir gitDir "objects"
        let _packDir = createSubdir objectDir "pack"
        let _infoDir = createSubdir objectDir "info"
        let refsDir = createSubdir gitDir "refs"
        let _headsDir = createSubdir refsDir "heads"
        let _tagsDir = createSubdir refsDir "tags"

        make dir |> Option.get |> Ok
