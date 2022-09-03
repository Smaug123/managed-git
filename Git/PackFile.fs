namespace Git

open System
open System.IO
open System.IO.Abstractions
open Git.Internals
open Force.Crc32
open ICSharpCode.SharpZipLib.Zip.Compression
open ICSharpCode.SharpZipLib.Zip.Compression.Streams

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
            ObjectChecksums : uint32[]
            /// 20-byte checksum of entire pack file
            Checksum : byte[]
        }

[<RequireQualifiedAccess>]
module PackFile =

    /// Returns the int, and the bytes read (for CRC32 purposes)
    let private readSizeEncodedInt (s : Stream) : int64 * byte[] =
        let rec go (count : int) (bytes : ResizeArray<byte>) (ans : int64) =
            let b = s.ReadByte ()

            if b < 0 then
                failwith "File ended while reading size encoded int"

            let ans = ans + (int64 (b % 128) <<< (count * 7))
            bytes.Add (byte b)

            if b >= 128 then
                go (count + 1) bytes ans
            else
                ans, bytes.ToArray ()

        go 0 (ResizeArray ()) 0L

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

    type Preamble =
        /// 20-byte name
        | BaseObjectName of byte[]
        | Offset of int64

    [<RequireQualifiedAccess>]
    type PackObject =
        | Object of Git.Object
        | Delta of Preamble

    /// If this was the last object, i.e. if untilPos was None, returns the 20-byte footer.
    /// TODO fix up the domain so that the None case is a separate function
    let private parseObject (untilPos : int64 option) (expectedCrc : uint32) (s : Stream) : PackObject * byte[] option =
        let firstByte = s.ReadByte ()

        if firstByte < 0 then
            failwith "expected to read an object, but file ended"

        let firstByte = byte firstByte

        let objectType =
            Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<byte, PackObjectType> ((firstByte >>> 4) % 8uy)

        let startSize = firstByte % 16uy

        let size, header =
            if firstByte >= 128uy then
                let output, bytes = readSizeEncodedInt s
                (output <<< 4) + (int64 startSize), [| yield firstByte ; yield! bytes |]
            else
                int64 startSize, [| firstByte |]

        let preamble =
            match objectType with
            | PackObjectType.ObjOfsDelta ->
                readSizeEncodedInt s
                |> fst
                |> Preamble.Offset
                |> Some
            | PackObjectType.ObjRefDelta ->
                Stream.consume s 20
                |> Preamble.BaseObjectName
                |> Some
            | _ -> None

        let object, footer =
            match untilPos with
            | None ->
                let finalObjectAndFooter = Stream.consumeToEnd s

                finalObjectAndFooter.[0 .. finalObjectAndFooter.Length - 21],
                Some finalObjectAndFooter.[finalObjectAndFooter.Length - 20 ..]
            | Some untilPos ->
                let numToConsume = int (untilPos - s.Position)

                if numToConsume < 0 then
                    failwith "Internal error: object too large for this implementation to consume"

                Stream.consume s numToConsume, None

        // TODO - check CRCs, this is currently failing
        //let obtainedCrc = Crc32Algorithm.Compute (object)
        //if obtainedCrc <> expectedCrc then
        //    failwithf "Compressed object had unexpected CRC. Expected: %i. Got: %i" expectedCrc obtainedCrc

        use objectStream = new MemoryStream (object)
        use s = new InflaterInputStream (objectStream, Inflater ())
        use resultStream = new MemoryStream ()
        s.CopyTo resultStream
        let decompressedObject = resultStream.ToArray ()

        if decompressedObject.LongLength <> size then
            failwithf "Object had unexpected length. Expected: %i. Got: %i" size decompressedObject.LongLength

        let toRet =
            match objectType, preamble with
            | PackObjectType.ObjBlob, None ->
                Object.Blob decompressedObject
                |> PackObject.Object
            | PackObjectType.ObjCommit, None ->
                Commit.decode decompressedObject
                |> Object.Commit
                |> PackObject.Object
            | PackObjectType.ObjTree, None ->
                Tree.decode decompressedObject
                |> Object.Tree
                |> PackObject.Object
            | PackObjectType.ObjOfsDelta, Some preamble -> PackObject.Delta preamble
            | PackObjectType.ObjRefDelta, Some preamble -> PackObject.Delta preamble
            | PackObjectType.ObjTag, None ->
                Tag.decode decompressedObject
                |> Object.Tag
                |> PackObject.Object
            | PackObjectType.ObjBlob, Some _
            | PackObjectType.ObjTag, Some _
            | PackObjectType.ObjTree, Some _
            | PackObjectType.ObjCommit, Some _ ->
                failwith "Logic error in this library, got a preamble for an unexpected object type"
            | PackObjectType.ObjRefDelta, None
            | PackObjectType.ObjOfsDelta, None ->
                failwith "Logic error in this library, got no preamble for a delta type"
            | _, _ -> failwith "Unexpected object type"

        toRet, footer

    type private HeaderMetadata =
        {
            Stream : Stream
            NumberOfObjects : uint32
        }

    let private readAndValidateHeader (file : IFileInfo) : HeaderMetadata =
        let s = file.OpenRead ()
        let header = Stream.consume s 4

        if header <> [| 80uy ; 65uy ; 67uy ; 75uy |] then
            // "PACK"
            failwithf "Invalid pack file header: %+A" header

        let versionBytes = Stream.consume s 4
        let version = toUint versionBytes

        if version <> 2u then
            failwithf "Unsupported pack file version %i" version

        let objectNumBytes = Stream.consume s 4
        let numberOfObjects = toUint objectNumBytes

        {
            Stream = s
            NumberOfObjects = numberOfObjects
        }

    let readAll (file : IFileInfo) (index : PackIndex) =
        let header = readAndValidateHeader file

        use stream = header.Stream

        let objectPositions = index.Offsets |> Array.map int64 |> Array.sort

        index.ObjectChecksums
        |> Array.map (fun crc ->
            let nextObjectIndex =
                // TODO probably binary search this, or maintain an incrementing
                // counter
                objectPositions
                |> Array.tryFindIndex (fun pos -> pos > stream.Position)

            // Account for the case where the index file contains garbage
            let startingIndex =
                match nextObjectIndex with
                | None -> objectPositions.[objectPositions.Length - 1]
                | Some 0 -> stream.Position
                | Some i -> objectPositions.[i - 1]

            if startingIndex <> stream.Position then
                stream.Seek (startingIndex, SeekOrigin.Begin)
                |> ignore

            let nextObjectPosition =
                nextObjectIndex
                |> Option.map (fun i -> objectPositions.[i])

            parseObject nextObjectPosition crc stream
        )

    type private Compare =
        | Less
        | Greater
        | Equal

    let private binarySearch<'chop, 'a>
        (get : 'chop -> 'a)
        (compare : 'a -> 'a -> Compare)
        (mean : 'chop -> 'chop -> 'chop)
        (needle : 'a)
        : 'chop -> 'chop -> 'chop option
        =
        let rec go (startPoint : 'chop) (endPoint : 'chop) =
            let start = get startPoint

            match compare start (get endPoint) with
            | Compare.Greater -> None
            | Compare.Equal ->
                if compare start needle = Compare.Equal then
                    Some startPoint
                else
                    None
            | Compare.Less ->

            let mean = mean startPoint endPoint

            match compare (get mean) needle with
            | Compare.Equal -> Some mean
            | Compare.Greater -> go startPoint mean
            | Compare.Less -> go mean endPoint

        go

    let private consumeOffset (s : Stream) =
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

    let readIndex (file : IFileInfo) : PackIndex =
        use s = file.OpenRead ()
        let header = Stream.consume s 4

        if header <> [| 255uy ; 116uy ; 79uy ; 99uy |] then
            failwithf "Invalid pack file header, may indicate unsupported version: %+A" header

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
                "Internal error: we don't yet support pack files with more than %i entries (%s)"
                Int32.MaxValue
                file.FullName

        let totalObjectNumber = int totalObjectNumber

        let objectNames =
            fun _ -> Stream.consume s 20
            |> Array.init totalObjectNumber

        let crc =
            fun _ -> Stream.consume s 4 |> toUint
            |> Array.init totalObjectNumber

        let offsets =
            fun _ -> consumeOffset s
            |> Array.init totalObjectNumber

        let bytesConsumedSoFar =
            // 4 for the header, 4 for the version, 256 * 4 for the counts, 20 * (that) for the names, 4 * (that)
            // for the crc, 4 * (that) for the small offsets.
            4
            + 4
            + 256 * 4
            + 20 * totalObjectNumber
            + 4 * totalObjectNumber
            + 4 * totalObjectNumber
            |> uint64

        // Fortuitously, 20 + 20 is the size of the trailer, and that's divisible by 8,
        // so we can just read in all remaining 8-byte chunks.
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

    let locateObject (Hash object) (packIndex : IFileInfo) (packFile : IFileInfo) : PackObject option =
        // Locate the file within the index
        use packIndex = packIndex.OpenRead ()
        let header = Stream.consume packIndex 4

        if header <> [| 255uy ; 116uy ; 79uy ; 99uy |] then
            failwithf "Invalid pack file header, may indicate unsupported version: %+A" header

        let versionBytes = Stream.consume packIndex 4
        let version = toUint versionBytes

        if version <> 2u then
            failwithf "Unsupported pack index version %i" version

        let nameLookup = object.[0]

        let startAtThisPrefix, endOfThisPrefix =
            if nameLookup = 0uy then
                0L, Stream.consume packIndex 4 |> toUint |> int64
            else
                packIndex.Seek ((int64 (nameLookup - 1uy)) * 4L, SeekOrigin.Current)
                |> ignore

                let before = Stream.consume packIndex 4 |> toUint |> int64
                let after = Stream.consume packIndex 4 |> toUint |> int64
                before, after

        let totalCount =
            packIndex.Seek (4L + 4L + 255L * 4L, SeekOrigin.Begin)
            |> ignore

            Stream.consume packIndex 4 |> toUint |> int64

        let comparisonMemo = System.Collections.Generic.Dictionary ()

        let lookup (location : int64) : byte[] =
            match comparisonMemo.TryGetValue location with
            | true, v -> v
            | false, _ ->

            packIndex.Seek (4L + 4L + 256L * 4L + location * 20L, SeekOrigin.Begin)
            |> ignore

            let number = Stream.consume packIndex 20
            comparisonMemo.[location] <- number
            number

        let compare (name1 : byte[]) (name2 : byte[]) : Compare =
            let rec go (i : int) =
                if i >= 20 then Compare.Equal
                else if name1.[i] < name2.[i] then Compare.Less
                elif name1.[i] > name2.[i] then Compare.Greater
                else go (i + 1)

            go 0

        let location =
            binarySearch lookup compare (fun x y -> (x + y) / 2L) object startAtThisPrefix endOfThisPrefix

        match location with
        | None -> None
        | Some location ->

        packIndex.Seek (
            4L
            + 4L
            + 256L * 4L
            + totalCount * 24L
            + location * 4L,
            SeekOrigin.Begin
        )
        |> ignore

        let index = consumeOffset packIndex

        let index =
            match index with
            | PackIndexOffset.OffsetInPack i -> int64 i
            | PackIndexOffset.LayerFiveEntry entry ->
                packIndex.Seek (
                    4L
                    + 4L
                    + 256L * 4L
                    + totalCount * 28L
                    + (int64 entry) * 8L,
                    SeekOrigin.Begin
                )
                |> ignore

                Stream.consume packIndex 8 |> toUint64 |> int64

        use fi = packFile.OpenRead ()
        fi.Seek (index, SeekOrigin.Begin) |> ignore
        // TODO constrain where we're reading to, and find the CRC
        parseObject (Some fi.Length) 0u fi |> fst |> Some
