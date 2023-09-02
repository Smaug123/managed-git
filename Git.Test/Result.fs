namespace Git.Test

[<RequireQualifiedAccess>]
module Result =

    let get<'res, 'err> (r : Result<'res, 'err>) : 'res =
        match r with
        | Ok x -> x
        | Error e -> failwithf "Expected Ok, but got Error: %+A" e

    let getError<'res, 'err> (r : Result<'res, 'err>) : 'err =
        match r with
        | Error e -> e
        | Ok x -> failwithf "Expected Error, but got Ok: %+A" x
