namespace Git

type PackVerificationLine =
    {
        Object : Hash
        Type : ObjectType
        Metadata : PackObjectMetadata
    }

    override this.ToString () =
        let typeString = string<ObjectType> this.Type

        let padding =
            Array.create
                (ObjectType.Commit.ToString().Length
                 - typeString.Length)
                " "
            |> String.concat ""

        sprintf "%s %s%s %s" (Hash.toString this.Object) typeString padding (string<PackObjectMetadata> this.Metadata)

type PackVerificationLineDelta =
    {
        Details : PackVerificationLine
        Depth : int
        BaseSha : Hash
    }

    override this.ToString () =
        sprintf "%s %i %s" (string<PackVerificationLine> this.Details) (this.Depth + 1) (Hash.toString this.BaseSha)

type PackVerification =
    {
        NonDeltaCount : int
        /// The zeroth entry of this array is the number of deltas with chain length 1, for example.
        /// The sixth entry of this array is the number of deltas with chain length 7, for example.
        /// The array is only as long as it needs to be, so it might not have any elements.
        ChainCounts : int[]
        Lines : Choice<PackVerificationLine, PackVerificationLineDelta> array
        Repo : Repository
        Name : Hash
    }

    override this.ToString () =
        seq {
            yield!
                this.Lines
                |> Seq.map (fun line ->
                    match line with
                    | Choice1Of2 line -> string<PackVerificationLine> line
                    | Choice2Of2 line -> string<PackVerificationLineDelta> line
                )

            yield sprintf "non delta: %i object%s" this.NonDeltaCount (if this.NonDeltaCount = 1 then "" else "s")

            yield!
                this.ChainCounts
                |> Seq.mapi (fun index count ->
                    sprintf "chain length = %i: %i object%s" (index + 1) count (if count = 1 then "" else "s")
                )

            // TODO: this isn't quite the same as Git. Git will allow you to omit `.pack`,
            // and will give a different answer depending on where you called the command
            // from.
            yield sprintf ".git/objects/pack/pack-%s.pack: ok" (Hash.toString this.Name)
        }
        |> String.concat "\n"

[<RequireQualifiedAccess>]
module VerifyPack =

    /// The ID is e.g. "871a8f18e20fa6104dbd769a07ca12f832048d00"; so the pack file
    /// derived from the ID is `.git/objects/pack/pack-{id}.pack".
    let verify (repo : Repository) (idHash : Hash) : PackVerification =
        let fs = repo.Fs
        let packDir = fs.Path.Combine (Repository.gitDir(repo).FullName, "objects", "pack")
        let id = Hash.toString idHash

        let index =
            fs.Path.Combine (packDir, sprintf "pack-%s.idx" id)
            |> fs.FileInfo.FromFileName

        let packFile =
            fs.Path.Combine (packDir, sprintf "pack-%s.pack" id)
            |> fs.FileInfo.FromFileName

        let allPacks =
            PackFile.readIndex index
            |> PackFile.readAll packFile

        let rec baseObject (o : PackObject) =
            match o with
            | PackObject.Object (object, name, _) -> object, name, 0
            | PackObject.Delta (object, _, name, _) ->
                let object, _, depth = baseObject object
                object, name, depth + 1

        let lines =
            allPacks
            |> Array.map (fun object ->
                match object with
                | PackObject.Object (object, name, metadata) ->
                    let objectType = Object.getType object

                    {
                        Object = name
                        Type = objectType
                        Metadata = metadata
                    }
                    |> Choice1Of2
                | PackObject.Delta (object, _diff, name, metadata) ->
                    let fullyResolvedBase, fullyResolvedBaseName, depth = baseObject object
                    let objectType = Object.getType fullyResolvedBase

                    {
                        Details =
                            {
                                Object = name
                                Type = objectType
                                Metadata = metadata
                            }
                        Depth = depth
                        BaseSha = fullyResolvedBaseName
                    }
                    |> Choice2Of2
            )

        lines
        |> Array.sortInPlaceBy (
            function
            | Choice1Of2 l -> l.Metadata.OffsetInPackFile
            | Choice2Of2 l -> l.Details.Metadata.OffsetInPackFile
        )

        // TODO(perf): everything from here onward is monstrously inefficient as a way of collecting chain counts
        let nonDeltaCount, chainCounts =
            ((0, Map.empty), lines)
            ||> Array.fold (fun (nonDeltaCount, chainCounts) line ->
                match line with
                | Choice1Of2 _ -> nonDeltaCount + 1, chainCounts
                | Choice2Of2 delta ->
                    // If we had F#.Core v5, we could use Map.change
                    let newMap =
                        match Map.tryFind delta.Depth chainCounts with
                        | None -> Map.add delta.Depth 1 chainCounts
                        | Some v -> Map.add delta.Depth (v + 1) chainCounts

                    nonDeltaCount, newMap
            )

        let maxChainLength = chainCounts |> Map.toSeq |> Seq.last |> fst

        let chainCounts =
            fun length ->
                Map.tryFind length chainCounts
                |> Option.defaultValue 0
            |> Array.init (maxChainLength + 1) // for the 0 index

        {
            NonDeltaCount = nonDeltaCount
            ChainCounts = chainCounts
            Lines = lines
            Repo = repo
            Name = idHash
        }
