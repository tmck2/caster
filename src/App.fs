module App

open Fable.Core
open Fable.Import.JS
open Fable.Core.JsInterop
open Fable.Import.Browser
open Math
open Model
open Graphics2d
open OverviewRenderer

let initState:Model.GameState = {
    Ticks = 0.
    Player = {
        Position = { x = 64.; y = 64. }
        Direction = { x = 0.; y = -1. }
    }
    CameraPlane = { x = 0.66; y = 0. }
    Level = { Map = [] }
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
    let s = (t-gameState.Ticks)/100.
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
            Vec2.rotate (-s/2.) dir 
        elif Keyboard.LeftPressed then
            Vec2.rotate (s/2.) dir
        else
            dir

    let updated_camera =
        if Keyboard.RightPressed then
            Vec2.rotate (-s/2.) camera 
        elif Keyboard.LeftPressed then
            Vec2.rotate (s/2.) camera
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

    let {Player=player;CameraPlane=c} = updatedState
    let {Position=p;Direction=r} = player
    let off = {x=200.;y=100.}
    let size = {x=128.;y=128.}

    let paintIntersection x =
        match x with
        | Some i -> gfx.fillCircle (i+off) 3. "red"
        | None -> ()

    let intersectWithSquare r =
        intersect {x=0.;y=128.} {x=0.;y= 1.} p r |> paintIntersection
        intersect {x=128.;y=0.} {x=1.; y=0.} p r |> paintIntersection
        intersect {x=0.;y=0.} {x= -1.;y= 0.} p r |> paintIntersection
        intersect {x=0.;y=0.} {x= 0.;y= -1.} p r |> paintIntersection

    let numRays = 15 

    [for i in 0..numRays do yield float(i)/float(numRays)] |>
    Seq.map (fun t -> ((1.0 - t) * (r - c)) + (t * (r + c)))
    |> Seq.iter intersectWithSquare

    gfx.strokeRect off size "white"
    [for i in 0..numRays do yield float(i)/float(numRays)] |>
    Seq.map (fun t -> ((1.0 - t) * (r - c)) + (t * (r + c)))
    |> Seq.iter (fun v -> gfx.strokeLine (p + off) (600. * v + p + off) "white")
    //gfx.strokeLine (p + off) (600. * r + p + off) "white"
    //gfx.strokeLine (p + off) (600. * (r - c) + p + off) "white"
    //gfx.strokeLine (p + off) (600. * (r + c) + p + off) "white"
    gfx.strokeLine (p + off - (50. * c)) (p + off + (50. * c)) "white"

let rec gameLoop (gfx:Graphics2d) t gameState =
    let updatedState = update t gameState

    //OverviewRenderer.render gfx {x=0.; y=0.} updatedState
    test gfx updatedState

    window.requestAnimationFrame(fun t -> (gameLoop gfx t updatedState)) |> ignore

let canvas = document.querySelector(".view") :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
let gfx = new Graphics2d(ctx)

Keyboard.initKeyboard ()
gameLoop gfx 0. initState

