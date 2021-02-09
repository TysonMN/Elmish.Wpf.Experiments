module Elmish.ClipboardCommand.Core.AppPasteClipboard

open Elmish
open Elmish.WPF
open System.Windows

type Model =
    { Items: string list
      SelectedItem: string option
      CopiedItem: string option }

type CmdMsgs =
    | SetToClipboard of string

let init () =
    let items = [ "A"; "B"; "C" ]
    { Items =  items
      SelectedItem = None
      CopiedItem = None },
    []

type Msg =
    | SelectItem of string option
    | SetCopiedItem of string
    | Copy of string
    | Paste of string
    | CmdException of exn

let update msg model =
    match msg with
    | SelectItem x -> { model with SelectedItem = x }, []
    | SetCopiedItem x -> { model with CopiedItem = Some x }, []
    | Copy x -> { model with CopiedItem = Some x }, [ SetToClipboard x ]
    | Paste x -> { model with Items = List.append model.Items [ sprintf "%s+" x ] }, []
    | CmdException _ -> model, []

let ClipboardKey = "Some key for test"

let bindings () = [
    "Items" |> Binding.subModelSeq
        ( fun m -> m.Items
        , snd
        , id
        , snd
        , fun () -> [])

    "SelectedItem" |> Binding.subModelSelectedItem
        ( "Items"
        , fun m -> m.SelectedItem
        , SelectItem)

    "Copy" |> Binding.cmdIf (fun m ->
        match m.SelectedItem with
        | Some x -> Copy x |> Some
        | None -> None)

    "Paste" |> Binding.cmdIf (fun m -> m.CopiedItem |> Option.map Paste)
]

let checkClipborad () =
    if Clipboard.ContainsData(ClipboardKey)
    then Clipboard.GetData(ClipboardKey) :?> string |> Some
    else None

let keepCheckingClipboard dispatch =
    async {
        while true do
            checkClipborad ()
            |> Option.map SetCopiedItem
            |> Option.iter dispatch
            do! Async.Sleep 500
    } |> Async.StartImmediate

let bindCmd = function
    | SetToClipboard x ->
        Cmd.OfFunc.attempt
            (fun () -> Clipboard.SetData(ClipboardKey, x)) ()
            CmdException

let main fwkElement =
    Program.mkProgramWpfWithCmdMsg init update bindings bindCmd
    |> Program.withSubscription (fun _ -> Cmd.ofSub keepCheckingClipboard)
    |> Program.startElmishLoop
        { ElmConfig.Default with
            LogConsole = true
            Measure = true }
        fwkElement
