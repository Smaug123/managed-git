namespace Git.Test

open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module Resource =

    let get (name : string) : byte[] =
        let assembly = Assembly.GetExecutingAssembly ()

        use stream = assembly.GetManifestResourceStream (sprintf "Git.Test.%s" name)

        if obj.ReferenceEquals (stream, null) then
            failwithf "Could not find resource with name %s" name

        use ms = new MemoryStream ()
        stream.CopyTo ms
        ms.ToArray ()
