﻿module FunToolbox.FileSystem

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
        not ^ isEmpty path
        && isTrimmed path 
        && path |> Seq.forall isValidPathCharacter

    let isValidName (name: string) =
        not ^ isEmpty name
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

/// Representation of a path extension. Always includes the leading period (.). 
type Ext = 
    | Ext of string
    override this.ToString() = 
        this |> function Ext extension -> extension

module Ext = 

    let tryParse str = 
        match () with
        | _ when not ^ isValidName str
            -> Error ^ sprintf "'%s': extension contains invalid filename characters" str
        | _ when not ^ str.startsWith "."
            -> Ok ^ Ext("." + str)
        | _ -> Ok ^ Ext str

    /// Parses an extension string, must be trimmed and may include a leading period.
    let parse = 
        tryParse >> function
        | Ok r -> r
        | Error e -> failwith e

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

    /// Changes the extension of the path. The extension may include the leading perion (.).
    /// If the extension is empty, the current extension is removed from the path.        
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
    
    let ensureExists path = 
       if not ^ Path.fileExists path then
            failwithf "file '%s' does not exist" (string path)

    /// Returns the time the file was last written to in UTC. Throws an exception
    /// if the file does not exist.
    let lastWriteTimeUTC path = 
        ensureExists path
        File.GetLastWriteTimeUtc(string path)

    /// Returns the time the file was created in UTC. Throws an exception
    /// if the file does not exist.
    let creationTimeUTC path =
        ensureExists path
        File.GetCreationTimeUtc(string path)
