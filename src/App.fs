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
        Position = { x = 64.; y = 32. }
        Direction = { x = 0.; y = -1. }
    }
    CameraPlane = { x = 0.66; y = 0. }
    Level = { Map = [
                Wall.create (384.,0.) (0.,0.)
                Wall.create (0.,0.) (0.,128.) 
                Wall.create (0.,128.) (0.,256.)
                Wall.create (0.,256.) (256.,256.)
                Wall.create (256.,256.) (256.,128.)
                Wall.create (256.,128.) (384.,128.)
                Wall.create (384.,128.) (384.,0.)
                Wall.create (96., 64.) (128., 64.)
                Wall.create (128.,64.) (128., 96.)
                Wall.create (128.,96.) (96.,96.)
                Wall.create (96.,96.) (96.,64.)
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
    gfx.fillRect {x=0.;y=0.} {x=800.;y=600.} "#263545"

    let {Player=player;CameraPlane=c;Level=level} = updatedState
    let {Position=p;Direction=r} = player
    let off = {x=200.;y=100.}
    let size = {x=128.;y=128.}

    let paintIntersection x =
        match x with
        | Some i -> gfx.fillCircle (i+off) 3. "red"
        | None -> ()

    let numRays = 100

    let intersectLevel level p r =
        level.Map
        |> Seq.map (fun w ->
                        (w,intersect w.Start (Vec2.perp (w.End-w.Start)) p r))
        |> Seq.map (fun (w,dist) ->
                        match dist with
                        | Some d -> 
                            let v = (d * r) + p
                            let ab = w.End - w.Start
                            let ac = w.End - v
                            let kac = Vec2.dot ab ac
                            let kab = Vec2.dot ab ab
                            if (Math.abs kac) <= 0.001 || (Math.abs (kac-kab)) <= 0.001 || (kac > 0. && kac<=kab) then
                                (d, Vec2.normalize (Vec2.perp ab))
                            else
                                (1000., {x=0.;y=0.})
                        | None -> (1000., {x=0.;y=0.}))
        |> Seq.filter (fun (d,v) -> d >= 0.)
        |> Seq.minBy (fun (d,v) -> d)

    let height d = 1./d * (64. * 150.)

    gfx.strokeRect {x=500.; y=250.} {x=300.;y=300.} "white"

    let light_dir = Vec2.normalize {x = -1.;y = -2.}

    let w = 300. / float(numRays)

    [for i in 0..numRays do yield (i, float(i)/float(numRays))]
    |> Seq.map (fun (i, t) -> (i, ((1.0 - t) * (r - c)) + (t * (r+c))))
    |> Seq.iter (fun (i, ray) -> 
        let (d,n) = intersectLevel level p ray
        let m = (d * ray) + p
        let x = m - p
        let v = Vec2.normalize c
        let proj = (Vec2.dot x v) * v
        let rej = Vec2.mag (x - proj)
        let h = (height rej) / 2.
        gfx.strokeLine (p + off) (m + off) "white"

        let mutable c = 128
        let b = (Vec2.dot n light_dir) * 128.
        if (b > 0.) then c <- c + int(128. * b)

        //gfx.strokeLine {x=500. + float(i); y=400. - h} {x=500. + float(i);y=400. + h} (sprintf "rgb(%i,%i,%i)" c c c)
        let clr = sprintf "rgb(%i,%i,%i)" c c c
        gfx.fillRect {x=500. + float(i)*w;y=400. - h} {x=w;y=h*2.} clr
        gfx.strokeText {x=0.; y=64.+float(i)*16.} (sprintf "%A" rej)
    )
    
    level.Map
    |> Seq.iter (fun wall -> gfx.strokeLine (wall.Start + off) (wall.End + off) "white")

    gfx.strokeLine (p + off - (50. * c)) (p + off + (50. * c)) "white"

    gfx.strokeText {x=0.; y=16.} (sprintf "%A" p)
    gfx.strokeText {x=0.; y=32.} (sprintf "%A" r)
    gfx.strokeText {x=0.; y=48.} (sprintf "%A" c)

let rec gameLoop (gfx:Graphics2d) t gameState =
    let updatedState = update t gameState

    test gfx updatedState

    window.requestAnimationFrame(fun t -> (gameLoop gfx t updatedState)) |> ignore

let canvas = document.querySelector(".view") :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
let gfx = new Graphics2d(ctx)

Keyboard.initKeyboard ()
gameLoop gfx 0. initState

