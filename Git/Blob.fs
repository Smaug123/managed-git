﻿namespace Git

[<RequireQualifiedAccess>]
module Blob =
    let encode (content : byte array) : byte array = content
    let decode (file : byte array) : byte array = file
