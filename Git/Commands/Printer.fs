namespace Git.Commands

type Printer =
    {
        WriteLine : string -> unit
    }

[<RequireQualifiedAccess>]
module Printer =
    let make () =
        {
            WriteLine = System.Console.WriteLine
        }
