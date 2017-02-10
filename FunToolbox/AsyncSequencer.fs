/// An agent that runs async message dispatch functions one after another. 
/// This is used to group async functions so that they can not run at the same time 
/// without using low level synchronization primitives.
module FunToolbox.AsyncSequencer

open Prelude
open System.Runtime.ExceptionServices

type Reply = 
    | Ok of obj
    | Error of ExceptionDispatchInfo

type Message =
    | RunAsync of (unit -> Async<obj>) * AsyncReplyChannel<Reply>

type AsyncSequencer = MailboxProcessor<Message>

let create() : AsyncSequencer =
    let agent = Agent.Start ^ fun _ -> async.Return()

    let rec loop() = async {
        let! (RunAsync(job, reply)) = agent.Receive()
        try
            let! r = job()
            reply.Reply ^ Ok r
        with e ->
            reply.Reply ^ Error ^ ExceptionDispatchInfo.Capture e
        return! loop()
    }

    Async.Start ^ loop()
    agent

let sequencify (sequencerAgent: _ agent) (job: 'msg -> Async<'r>) msg : Async<'r> = 
        
    // this first part runs synchronously in the context of the service dispatcher. 
    // So to properly run async functions in sequence we need to immediately put the message 
    // into the agent's queue here and run the job _after_ the message was delivered.

    sequencerAgent.PostAndAsyncReply ^ 
        fun rc -> RunAsync((fun () -> job msg |> Async.map box), rc)
    |> Async.map ^ 
    function
    | Ok o -> 
        unbox o
    | Error e -> 
        e.Throw()
        Unchecked.defaultof<_>