namespace FunToolbox.Tests

open NUnit.Framework
open FsUnit
open System.IO

open FunToolbox
open System.Threading

[<TestFixture>]
type AtomFileTests() =

    [<Test>]
    member this.wouldBlockForeverWhenFileIsLocked() = 
        let fn = "test.atomfile"
        use fs = File.Open(fn, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)

        let thread = new Thread(fun () -> AtomFile.swap id fn)
        thread.Start()
        Thread.Sleep(100)
        thread.IsAlive |> should be True
        thread.Abort()
        thread.Join()
