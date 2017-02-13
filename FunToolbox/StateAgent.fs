/// An agent that maintains a state and runs modifies the state with async messages.

[<Prelude.RQA>]
module FunToolbox.StateAgent

open Prelude
open System.Runtime.ExceptionServices

type Reply = 
    | Ok
    | Error of ExceptionDispatchInfo

type Message<'state> =
    | RunAsync of ('state -> Async<'state>) * AsyncReplyChannel<Reply>

type Agent<'state>(mb: MailboxProcessor<Message<'state>>) =
    member internal this.MB = mb

let create (initial: 'state) : Agent<'state> =

    let agent = Agent.Start ^ fun _ -> async.Return()

    let rec loop state = async {
        let! (RunAsync(job, reply)) = agent.Receive()
        try
            let! newState = job state
            reply.Reply ^ Ok
            return! loop newState
        with e ->
            reply.Reply ^ Error ^ ExceptionDispatchInfo.Capture e
            return! loop state
    }

    Async.Start ^ loop initial
    Agent<_>(agent)

type Agent<'state> with
    /// Execute the state update function f inside the agent's context.
    member this.Update f =
        this.MB.PostAndAsyncReply ^ fun rc -> RunAsync(f, rc)
        |> Async.map ^
            function
            | Error e -> e.Throw()
            | Ok -> ()

    member this.Update f =
        fun state -> async {
            return f state
        }
        |> this.Update

