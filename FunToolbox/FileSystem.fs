module FunToolbox.FileSystem

open System
open System.IO
open Prelude

[<AutoOpen>]
module private Helpers =
    let normalizedPath (path: string) = 
        path.Replace("\\", "/")

    let isEmpty (str: string) = str.Length = 0

    let isTrimmed (str: string) = str.Trim() = str

    let isValidPathCharacter = 
        let invalidPathChars = Path.GetInvalidPathChars()
        fun c ->
            invalidPathChars
            |> Array.contains c
            |> not

    let isValidNameCharacter =
        let invalidNameChars = Path.GetInvalidFileNameChars()
        fun c ->
            invalidNameChars
            |> Array.contains c
            |> not

    let isValidPath (path: string) = 
        isEmpty path |> not
        && isTrimmed path 
        && path |> Seq.forall isValidPathCharacter

    let isValidName (name: string) =
        isEmpty name |> not
        && isTrimmed name
        && name |> Seq.forall isValidNameCharacter

    let isValidAbsolutePath (path: string) = 
        isValidPath path && Path.IsPathRooted path
            
/// Representation of a path. Always an absolute, full, rooted path with normalized
/// Separators (all backslashes converted to forward slashes)
type Path = 
    | Path of string
    [<Obsolete("use (string path)")>]
    member this.value = string this
    override this.ToString() = 
        this |> function Path path -> path

/// Representation of a path extension. Does not include the leading period (.). 
/// Never empty.
type Ext = 
    | Ext of string
    override this.ToString() = 
        this |> function Ext extension -> extension

[<CR(ModuleSuffix)>]
module Path = 

    let parse str =
        let normalized = str |> normalizedPath
        if not <| Helpers.isValidAbsolutePath normalized then
            failwithf "'%s' is an invalid absolute path" normalized
        Path normalized

    let map (f: string -> string) (Path path) = 
        path |> f |> parse

    /// Extend the path with a sub-path. The sub-path can be a relative or
    /// absolute path.
    let extend (subPath: string) (path: Path) = 
        path 
        |> map ^ fun p -> Path.Combine(p, subPath)

    /// The parent / directory part of the path
    let parent (path: Path) = 
        path |> map Path.GetDirectoryName

    /// Returns the name part of the path.
    let name (Path path) = 
        Path.GetFileName path
           
    /// Split the path in a directory part and a name part.
    let split (path: Path) = 
        path |> parent, path |> name

    /// Changes the extension of the path. The extension is not specified with a dot.
    /// If the extension is empty, the current extension is removed from the path.        
    [<Obsolete("use withExt")>]
    let withExtension (ext: string) (path: Path) = 
        path
        |> map ^ fun p -> Path.ChangeExtension(p, ext)

    let withExt (Ext ext) (path: Path) =
        path
        |> map ^ fun p -> Path.ChangeExtension(p, ext)

    let removeExtension path =
        path
        |> map ^ fun p -> Path.ChangeExtension(p, "")

    let ensureDirectoryExists (Path path) = 
        Directory.CreateDirectory path |> ignore

    let ensureDirectoryOfPathExists (path: Path) =
        path |> parent |> ensureDirectoryExists
        
    let currentDirectory() : Path = 
        Directory.GetCurrentDirectory()
        |> parse
        
    let fileExists (Path path) = 
        File.Exists path
    
[<CR(ModuleSuffix)>]
module Ext = 

    /// Parses an extension string, must be trimmed and without a leading period, also
    /// is not allowed to contain any invalid file characters.
    let parse (ext: string) = 
        if ext.startsWith "." then
            failwithf "extension '%s' is not allowed to start with a period (.)" ext
        
        if not <| Helpers.isValidName ext then
            failwithf "'%s' is an invalid extension" ext

        Ext ext

module File = 

    let loadBinary (Path path) = 
        use stream = File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read ||| FileShare.Delete)
        if stream.Length > (int64 Int32.MaxValue) then
            failwith "stream too long"
        let array = Array.zeroCreate (int stream.Length)
        stream.Read(array, 0, array.Length) |> expect array.Length
        array
    
    let loadText (Path path) (encoding: System.Text.Encoding) =
        use stream = File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read ||| FileShare.Delete)
        use reader = new StreamReader(stream, encoding)
        reader.ReadToEnd()


