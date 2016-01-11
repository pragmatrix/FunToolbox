namespace FunToolbox.Tests

open NUnit.Framework
open FsUnit

open FunToolbox

open System
open System.IO
open System.Threading
open System.Text

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

    [<Test>]
    member this.counterTest() = 
        let fn = "count.atomfile"

        let swapFunction count = 
            match count with
            | None -> 1 |> string |> Some
            | Some str -> UInt32.Parse(str) + 1u |> string |> Some

        let swapFunction = AtomFile.Swapper.string Encoding.UTF8 swapFunction

        let rec countAddThread() = 
            AtomFile.swap swapFunction fn
            countAddThread()

        let thread1 = new Thread(countAddThread)
        let thread2 = new Thread(countAddThread)
        thread1.Start()
        thread2.Start()

        Thread.Sleep(100)
        thread1.Abort()
        thread2.Abort()
        thread1.Join()
        thread2.Join()

        let data = AtomFile.value fn
        let value = data.Value |> fun bytes -> Encoding.UTF8.GetString(bytes)
        System.Console.WriteLine("counter: " + value)