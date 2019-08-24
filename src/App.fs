module App

open Fable.Core
open Fable.Import.JS
open Fable.Core.JsInterop
open Fable.Import.Browser
open Math
open Model
open Graphics2d

let initState:Model.GameState = {
    Ticks = 0.
    Player = {
        Position = { x = 64.; y = 64. }
        Direction = { x = 0.; y = -1. }
    }
    CameraPlane = { x = 0.66; y = 0. }
    Level = { Map = [
                Wall.create (256.,0.) (0.,0.)
                Wall.create (0.,0.) (0.,128.) 
                Wall.create (0.,128.) (128.,256.)
                Wall.create (128.,256.) (256.,256.)
                Wall.create (256.,256.) (256.,0.)
            ] }
}

module Keyboard =
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

let update t gameState =
    let s = (t-gameState.Ticks)/20.
    let {CameraPlane=camera; Player=player} = gameState
    let {Position=pos; Direction=dir} = player

    let updated_pos =
        if Keyboard.UpPressed then
            pos + (s * dir)
        elif Keyboard.DownPressed then
            pos + (-s * dir)
        else
            pos

    let updated_dir =
        if Keyboard.RightPressed then
            Vec2.rotate (-s/8.) dir 
        elif Keyboard.LeftPressed then
            Vec2.rotate (s/8.) dir
        else
            dir

    let updated_camera =
        if Keyboard.RightPressed then
            Vec2.rotate (-s/8.) camera 
        elif Keyboard.LeftPressed then
            Vec2.rotate (s/8.) camera
        else
            camera

    { gameState with
        Player={gameState.Player with
                    Position = updated_pos
                    Direction = updated_dir}
        CameraPlane=updated_camera
        Ticks = t}

let test (gfx:Graphics2d) updatedState =
    gfx.fillRect {x=0.;y=0.} {x=640.;y=400.} "#263545"

    let {Player=player;CameraPlane=c;Level=level} = updatedState
    let {Position=p;Direction=r} = player
    let off = {x=200.;y=100.}
    let size = {x=128.;y=128.}

    let paintIntersection x =
        match x with
        | Some i -> gfx.fillCircle (i+off) 3. "red"
        | None -> ()

    let numRays = 15 

    let intersectLevel level p r =
        level.Map
        |> Seq.map (fun w ->
                        intersect w.Start (Vec2.perp (w.End-w.Start)) p r)
        |> Seq.map (fun dist ->
                        match dist with
                        | Some d -> d
                        | None -> 1000.)
        |> Seq.min

    [for i in 0..numRays do yield float(i)/float(numRays)]
    |> Seq.map (fun t -> ((1.0 - t) * (r - c)) + (t * (r+c)))
    |> Seq.iter (fun r -> 
        let d = intersectLevel level p r
        let v = (d * r) + p
        gfx.strokeLine (p + off) (v + off) "white")
    
    level.Map
    |> Seq.iter (fun wall -> gfx.strokeLine (wall.Start + off) (wall.End + off) "white")

    gfx.strokeLine (p + off - (50. * c)) (p + off + (50. * c)) "white"

let rec gameLoop (gfx:Graphics2d) t gameState =
    let updatedState = update t gameState

    test gfx updatedState

    window.requestAnimationFrame(fun t -> (gameLoop gfx t updatedState)) |> ignore

let canvas = document.querySelector(".view") :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
let gfx = new Graphics2d(ctx)

Keyboard.initKeyboard ()
gameLoop gfx 0. initState

