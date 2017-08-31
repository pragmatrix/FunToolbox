/// An agent that maintains a state and modifies the state with functions that
/// are delivered via async messages.
[<Prelude.RQA>]
module FunToolbox.StateAgent

open Prelude
open System.Runtime.ExceptionServices

type Reply = 
    | Ok
    | Error of ExceptionDispatchInfo

type Message<'state> =
    | RunAsync of ('state -> Async<'state>) * AsyncReplyChannel<Reply>
    | GetState of AsyncReplyChannel<'state>

type Agent<'state>(agent: Message<'state> agent) =
    member internal this.Agent = agent

let create (initial: 'state) : Agent<'state> =

    let agent = Agent.create()

    let rec loop state = async {
        let! msg = agent.Receive()
        match msg with
        | RunAsync(job, reply) ->
            try
                let! newState = job state
                reply.Reply ^ Ok
                return! loop newState
            with e ->
                reply.Reply ^ Error ^ ExceptionDispatchInfo.Capture e
                return! loop state
        | GetState reply ->
            reply.Reply state
            return! loop state
    }

    Async.Start ^ loop initial
    Agent<_>(agent)

type Agent<'state> with
    /// Execute the state update function f inside the agent's context.
    /// Note: even though Update returns an async, the ordering of the updates is synchronized 
    /// with the ordering of the update calls (the message that is sent to the mailboxprocess is placed
    /// in its queue before this function returns).
    member this.Update f =
        this.Agent.PostAndAsyncReply 
            ^ fun replyChannel -> RunAsync(f, replyChannel)
        |> Async.map ^
            function
            | Error e -> e.Throw()
            | Ok -> ()
    
    /// Get the current state.
    member this.State =
        this.Agent.PostAndAsyncReply 
            ^ fun replyChannel -> GetState(replyChannel)
