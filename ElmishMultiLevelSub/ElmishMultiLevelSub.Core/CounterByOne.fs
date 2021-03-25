﻿module CounterByOne

open Elmish.WPF

type Model = { Counter: int }

type Msg =
    | Increment

let init () = { Counter = 0 }

let update msg m =
    match msg with
    | Increment -> { m with Counter = m.Counter + 1 }

module Platform =

    let bindings () = [
        "Counter" |> Binding.oneWay (fun r -> r.Counter)
    ]

    [<RequireQualifiedAccess>]
    module CounterByOneSub =
        let sub dispatch =
            let timer = new System.Timers.Timer 100.
            timer.Elapsed.Add(fun _ -> dispatch Increment)
            timer.Start ()