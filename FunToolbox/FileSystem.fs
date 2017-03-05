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

    let invalidPathChars = Path.GetInvalidPathChars()
    let isValidPathCharacter (c: char) = 
        Array.IndexOf(invalidPathChars, c) = -1

    let isValidPath (path: string) = 
        isEmpty path |> not
        && isTrimmed path 
        && path |> Seq.forall isValidPathCharacter

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
        
    let withExtension (ext: string) (path: Path) = 
        path
        |> map ^ fun p -> Path.ChangeExtension(p, ext)
        
    let ensureDirectoryExists (Path path) = 
        Directory.CreateDirectory path |> ignore

    let ensureDirectoryOfPathExists (path: Path) =
        path |> parent |> ensureDirectoryExists
        
    let currentDirectory() : Path = 
        Directory.GetCurrentDirectory()
        |> parse
        
    let fileExists (Path path) = 
        File.Exists path