module App

open Fable.Core
open Fable.Import.JS
open Fable.Core.JsInterop
open Fable.Import.Browser
open Math
open Model
open Graphics2d

let NumRays = 640
let Width = 640.
let Height = 480.
let WallHeight = Height * 0.75

let initState:Model.GameState = {
    Ticks = 0.
    Player = {
        Position = { x = 8.; y = 11. }
        Direction = { x = 0.; y = -1. }
    }
    CameraPlane = { x = 0.66; y = 0. }
    Level = { Map = [
                Wall.create (38.4,0.) (0.,0.)
                Wall.create (0.,0.) (0.,12.8) 
                Wall.create (0.,12.8) (0.,25.6)
                Wall.create (0.,25.6) (25.6,25.6)
                Wall.create (25.6,25.6) (25.6,12.8)
                Wall.create (25.6,12.8) (38.4,12.8)
                Wall.create (38.4,12.8) (38.4,0.)
                
                Wall.create (9.6, 6.4) (12.8,6.4)
                Wall.create (12.8,6.4) (12.8,9.6)
                Wall.create (12.8,9.6) (9.6,9.6)
                Wall.create (9.6,9.6) (9.6,6.4)

                Wall.create (9.6,12.8) (12.8,12.8)
                Wall.create (12.8,12.8) (12.8,16.)
                Wall.create (12.8,16.) (9.6,16.)
                Wall.create (9.6,16.) (9.6,12.8)
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
    let s = (t-gameState.Ticks)/200.
    let sa = s/2.
    let {CameraPlane=camera; Player=player; Level=level} = gameState
    let {Position=pos; Direction=dir} = player

    let mutable updated_pos =
        if Keyboard.UpPressed then
            pos + (s * dir)
        elif Keyboard.DownPressed then
            pos + (-s * dir)
        else
            pos

    let levelCircleCollision pos =
        level.Map
        |> Seq.exists (fun w ->
            lineCircleCollision w.Start w.End pos 1.75 
        )

    if (levelCircleCollision updated_pos) then
        updated_pos <- pos

    let updated_dir =
        if Keyboard.RightPressed then
            Vec2.rotate (-sa) dir 
        elif Keyboard.LeftPressed then
            Vec2.rotate (sa) dir
        else
            dir

    let updated_camera =
        if Keyboard.RightPressed then
            Vec2.rotate (-sa) camera 
        elif Keyboard.LeftPressed then
            Vec2.rotate (sa) camera
        else
            camera

    { gameState with
        Player={gameState.Player with
                    Position = updated_pos
                    Direction = updated_dir}
        CameraPlane=updated_camera
        Ticks = t}

let overhead (gfx:Graphics2d) updatedState =
    gfx.fillRect {x=0.;y=0.} {x=800.;y=600.} "#263545"

    let {Player=player;CameraPlane=c;Level=level} = updatedState
    let {Position=p;Direction=r} = player
    let off = {x=575.;y=25.}

    let intersectLevel level p r =
        level.Map
        |> Seq.map (fun w ->
                        (w,intersect w.Start (Vec2.perp (w.End-w.Start)) p r))
        |> Seq.map (fun (w,dist) ->
                        match dist with
                        | Some d -> 
                            let v = (d * r) + p
                            if pointOnLine w.Start w.End v then
                                (d, Vec2.normalize (Vec2.perp (w.End-w.Start)))
                            else
                                (1000., {x=0.;y=0.})
                        | None -> (1000., {x=0.;y=0.}))
        |> Seq.filter (fun (d,v) -> d >= 0.)
        |> Seq.minBy (fun (d,v) -> d)

    let height d = 1./d * WallHeight

    gfx.fillRect {x=0.; y=0.} {x=Width;y=Height/2.} "rgb(48,48,48)"
    gfx.fillRect {x=0.; y=Height/2.} {x=Width;y=Height/2.} "rgb(64,64,64)"
    gfx.strokeRect {x=0.; y=0.} {x=Width;y=Height} "white"

    let light_dir = Vec2.normalize {x = -1.;y = -2.}

    let w = Width / float(NumRays)

    [for i in 0..NumRays do yield (i, float(i)/float(NumRays))]
    |> Seq.map (fun (i, t) -> (i, ((1.0 - t) * (r - c)) + (t * (r+c))))
    |> Seq.iter (fun (i, ray) -> 
        let (d,n) = intersectLevel level p ray
        let m = (d * ray) + p
        let x = m - p
        let v = Vec2.normalize c
        let proj = (Vec2.dot x v) * v
        let rej = Vec2.mag (x - proj)
        let h = height rej
        gfx.strokeLine (p + off) (m + off) "white"

        let mutable c = 128
        let b = (Vec2.dot n light_dir)
        if (b > 0.) then c <- c + int(b * 128.) else c <- c + int(b * -128.)

        let clr = sprintf "rgb(%i,%i,%i)" c c c
        gfx.fillRect {x=0. + float(i) * w; y=0. + Height/2. - h} {x=w;y=h*2.} clr
    )
    
    level.Map
    |> Seq.iter (fun wall -> gfx.strokeLine (wall.Start + off) (wall.End + off) "white")

    gfx.strokeText {x=8.; y=16.} (sprintf "%A" p)
    gfx.strokeText {x=8.; y=32.} (sprintf "%A" r)
    gfx.strokeText {x=8.; y=48.} (sprintf "%A" c)

let rec gameLoop (gfx:Graphics2d) t gameState =
    let updatedState = update t gameState

    overhead gfx updatedState

    window.requestAnimationFrame(fun t -> (gameLoop gfx t updatedState)) |> ignore

let canvas = document.querySelector(".view") :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
let gfx = new Graphics2d(ctx)

Keyboard.initKeyboard ()
gameLoop gfx 0. initState

