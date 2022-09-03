namespace Git

open System
open System.IO
open System.IO.Abstractions
open Git.Internals
open Force.Crc32

type PackObjectType =
    | ObjCommit = 1uy
    | ObjTree = 2uy
    | ObjBlob = 3uy
    | ObjTag = 4uy
    /// Encodes the base object's offset in the pack file.
    | ObjOfsDelta = 6uy
    /// Encodes the name of the base object.
    | ObjRefDelta = 7uy

type private PackIndexOffset =
    /// This entry is indicating that the corresponding value can be found in the pack file
    /// at the given offset.
    | OffsetInPack of uint32
    /// This entry is indicating that the index's fifth layer contains the offset, and it's
    /// at this offset within the fifth layer.
    | LayerFiveEntry of uint32

type PackIndex =
    private
        {
            Names : byte[][]
            Offsets : uint64[]
            /// 20-byte CRCs of each object
            ObjectChecksums : byte[][]
            /// 20-byte checksum of entire pack file
            Checksum : byte[]
        }

[<RequireQualifiedAccess>]
module PackFile =

    let private readSizeEncodedInt (startWith : byte) (s : Stream) : int64 =
        let rec go (ans : int64) =
            let b = s.ReadByte ()

            if b < 0 then
                failwith "File ended while reading size encoded int"

            let ans = (ans <<< 7) + int64 (b % 128)
            if b >= 128 then go ans else ans

        go (int64 startWith)

    let private toUint (bytes : byte[]) : uint32 =
        let mutable ans = 0u

        for b in bytes do
            ans <- ans * 256u + uint32 b

        ans

    let private toUint64 (bytes : byte[]) : uint64 =
        let mutable ans = 0uL

        for b in bytes do
            ans <- ans * 256uL + uint64 b

        ans

    let private parseObject (s : Stream) =
        let firstByte = s.ReadByte ()

        if firstByte < 0 then
            failwith "expected to read an object, but file ended"

        let firstByte = byte firstByte

        let objectType =
            Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<byte, PackObjectType> ((firstByte >>> 4) % 8uy)

        let startSize = firstByte % 16uy

        let size =
            if firstByte >= 128uy then
                readSizeEncodedInt startSize s
            else
                int64 startSize

        ()

    let read (file : IFileInfo) (index : PackIndex) =
        use s = file.OpenRead ()
        let header = Stream.consume s 4

        if header <> [| 80uy ; 65uy ; 67uy ; 75uy |] then
            // "PACK"
            failwithf "Invalid pack file header: %+A" header

        let versionBytes = Stream.consume s 4
        let version = toUint versionBytes

        if version <> 2u then
            failwithf "Unsupported packfile version %i" version

        let sizeBytes = Stream.consume s 4
        let size = toUint sizeBytes
        let objectNumBytes = Stream.consume s 4
        let numberOfObjects = toUint objectNumBytes

        parseObject s
        ()

    let readIndex (file : IFileInfo) : PackIndex =
        use s = file.OpenRead ()
        let header = Stream.consume s 4

        if header <> [| 255uy ; 116uy ; 79uy ; 99uy |] then
            failwithf "Invalid packfile header, may indicate unsupported version: %+A" header

        let versionBytes = Stream.consume s 4
        let version = toUint versionBytes

        if version <> 2u then
            failwithf "Unsupported pack index version %i" version

        // Explanation from https://codewords.recurse.com/issues/three/unpacking-git-packfiles:
        // the N-th entry of this table records the number of objects in the corresponding pack,
        // the first byte of whose object name is less than or equal to N.
        // For example, if the first entry has value 4, then we eventually expect to find four objects in the
        // pack whose 20-byte object name (derived from SHA hashes) starts with 00.
        // Then if the second entry has value 4, we eventually expect to find no objects starting 01, because
        // the count is cumulative.
        let countsOfEachName =
            let arr = Array.zeroCreate<uint32> 256

            for i in 0..255 do
                arr.[i] <- Stream.consume s 4 |> toUint

            arr

        let totalObjectNumber = Array.last countsOfEachName

        if int64 totalObjectNumber > int64 Int32.MaxValue then
            failwithf
                "Internal error: we don't yet support packfiles with more than %i entries (%s)"
                Int32.MaxValue
                file.FullName

        let totalObjectNumber = int totalObjectNumber

        let objectNames =
            fun _ -> Stream.consume s 20
            |> Array.init totalObjectNumber

        let crc =
            fun _ -> Stream.consume s 4
            |> Array.init totalObjectNumber

        let offsets =
            fun _ ->
                let firstByte = s.ReadByte ()

                if firstByte < 0 then
                    failwith "expected to read an offset, but got end of file"

                let firstByte = byte firstByte
                let remainingBytes = Stream.consume s 3

                if firstByte >= 128uy then
                    toUint remainingBytes
                    + ((uint32 (firstByte % 128uy)) <<< 24)
                    |> PackIndexOffset.LayerFiveEntry
                else
                    toUint remainingBytes
                    + ((uint32 firstByte) <<< 24)
                    |> PackIndexOffset.OffsetInPack
            |> Array.init totalObjectNumber

        let bytesConsumedSoFar =
            // 4 for the header, 4 for the version, 256 * 4 for the counts, 20 * (that) for the names, 4 * (that)
            // for the crc, 4 * (that) for the small offsets.
            4
            + 4
            + (256 * 4)
            + 20 * totalObjectNumber
            + 4 * totalObjectNumber
            + 4 * totalObjectNumber
            |> uint64

        // Fortuitously, 20 + 20 is the size of the trailer, and that's divisible by 8.
        let buffer = Array.zeroCreate<byte> 8
        let entries = ResizeArray<byte[]> ()

        while s.Read (buffer, 0, 8) = 8 do
            entries.Add (buffer.Clone () |> unbox<byte[]>)

        let checksumPackFile = Array.zeroCreate<byte> 20
        Array.Copy (entries.[entries.Count - 5], 0, checksumPackFile, 0, 8)
        Array.Copy (entries.[entries.Count - 4], 0, checksumPackFile, 8, 8)
        Array.Copy (entries.[entries.Count - 3], 0, checksumPackFile, 16, 4)
        let checksumIndexFile = Array.zeroCreate<byte> 20
        Array.Copy (entries.[entries.Count - 3], 4, checksumIndexFile, 0, 4)
        Array.Copy (entries.[entries.Count - 2], 0, checksumIndexFile, 4, 8)
        Array.Copy (entries.[entries.Count - 1], 0, checksumIndexFile, 12, 8)

        // TODO: validate the index file checksum

        let longEntries =
            fun i ->
                let rawOffset = entries.[i] |> toUint64
                rawOffset - bytesConsumedSoFar
            |> Array.init (entries.Count - 5)

        let offsets =
            offsets
            |> Array.map (fun offset ->
                match offset with
                | OffsetInPack i -> uint64 i
                | LayerFiveEntry i -> longEntries.[int i]
            )

        {
            Offsets = offsets
            Names = objectNames
            Checksum = checksumPackFile
            ObjectChecksums = crc
        }
