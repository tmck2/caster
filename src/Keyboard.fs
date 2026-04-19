module Keyboard

open Fable.Core.JsInterop
open Browser.Types
open Browser.Dom

let LeftKey = 37
let UpKey = 38
let RightKey = 39
let DownKey = 40
let CKey = 67

let Keys = Array.create 256 false

let update (e : KeyboardEvent, pressed) =
    let keyCode = int e.keyCode
    Keys.[keyCode] <- pressed
    
let initKeyboard () =
    document.addEventListener("keydown", fun e -> update(e :?> KeyboardEvent, true))
    document.addEventListener("keyup", fun e -> update(e :?> KeyboardEvent, false))
