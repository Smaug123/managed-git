namespace Git

[<RequireQualifiedAccess>]
module Blob =
    let create (content : byte array) =
        {
            Header = Header.Blob content.Length
            Content = content
        }
