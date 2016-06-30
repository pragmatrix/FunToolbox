namespace FunToolbox.Tests

open System
open System.IO
open System.Threading
open System.Text
open NUnit.Framework
open FsUnit
open FunToolbox

[<TestFixture>]
type AtomFileTests() =

    [<Test>]
    member this.wouldBlockForeverWhenFileIsLocked() = 
        let fn = "test.atomfile"
        use fs = File.Open(fn, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)

        let thread = new Thread(fun () -> AtomFile.swap id fn |> ignore)
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

        let cancellation = new CancellationTokenSource()
        let token = cancellation.Token

        let minimumRounds = 100

        let rec countAddThread cnt = 
            if token.IsCancellationRequested && cnt > minimumRounds then
                ()
            else
            AtomFile.swap swapFunction fn |> ignore
            countAddThread (cnt+1)


        let thread1 = new Thread(fun () -> countAddThread 0)
        let thread2 = new Thread(fun () -> countAddThread 0)
        thread1.Start()
        thread2.Start()

        Thread.Sleep(100)
        cancellation.Cancel()
        thread1.Join()
        thread2.Join()

        let data = AtomFile.read fn
        data.IsSome |> should be True
        let value = data.Value |> fun bytes -> Encoding.UTF8.GetString(bytes)
        System.Console.WriteLine("counter: " + value)