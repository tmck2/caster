module Keyboard

open Fable.Core.JsInterop
open Fable.Import.Browser

let mutable LeftPressed = false
let mutable UpPressed = false
let mutable RightPressed = false
let mutable DownPressed = false

let update (e : KeyboardEvent, pressed) =
    let keyCode = int e.keyCode
    match keyCode with
    | 37 -> LeftPressed <- pressed
    | 38 -> UpPressed <- pressed
    | 39 -> RightPressed <- pressed
    | 40 -> DownPressed <- pressed
    | _ -> ()

let initKeyboard () =
    document.addEventListener("keydown", !^(fun e -> update(e :?> _, true)))
    document.addEventListener("keyup", !^(fun e -> update(e :?> _, false)))
