namespace Git.Test

open Git.Commands

[<RequireQualifiedAccess>]
module Printer =

    let makeTest () : Printer * (unit -> string list) =
        let stdout = ResizeArray ()

        let p =
            {
                WriteLine = fun s -> lock stdout (fun () -> stdout.Add s)
            }

        let freezeStdout () =
            lock stdout (fun () -> Seq.toList stdout)

        p, freezeStdout
