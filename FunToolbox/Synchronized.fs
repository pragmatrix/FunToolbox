module FunToolbox.Synchronized

open System.Threading

// ConcurrentQueue does not quite support what we want here.
// duplicated from the IVR project (Threading.SynchronizedQueue<_>)
    
type Queue<'t>() = 
    let monitor = obj()
    let queue = System.Collections.Generic.Queue<'t>()
        
    member this.enqueue v = 
        lock monitor (
            fun () -> 
                queue.Enqueue v
                if queue.Count = 1 then
                    Monitor.Pulse monitor
            )

    member this.dequeue() =
        lock monitor (
            fun () ->
                while queue.Count = 0 do
                    Monitor.Wait monitor |> ignore

                queue.Dequeue()
            )