module FunToolbox.Tests.AtomFileTests

open System
open System.IO
open System.Threading
open System.Text
open FsUnit
open Xunit
open FunToolbox

[<Fact>]
let wouldBlockForeverWhenFileIsLocked() = 
    let fn = "test.atomfile"
    use fs = File.Open(fn, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)

    let thread = Thread(fun () -> AtomFile.swap id fn |> ignore)
    thread.Start()
    Thread.Sleep(100)
    thread.IsAlive |> should be True
    // Can't use thread.Abort() this throws PlatformNotSupportedException
    // thread.Abort()
    // thread.Join()

[<Fact>]
let counterTest() = 
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


    let thread1 = Thread(fun () -> countAddThread 0)
    let thread2 = Thread(fun () -> countAddThread 0)
    thread1.Start()
    thread2.Start()

    Thread.Sleep(100)
    cancellation.Cancel()
    thread1.Join()
    thread2.Join()

    let data = AtomFile.read fn
    data.IsSome |> should be True
    let value = data.Value |> fun bytes -> Encoding.UTF8.GetString(bytes)
    Console.WriteLine("counter: " + value)
