module App

open Fable.Import.Browser
open Math
open Model
open Level
open Graphics2d
open Keyboard

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
    Level = new Level()
}

let update t gameState =
    let s = (t-gameState.Ticks)/200.
    let sa = s/2.
    let {CameraPlane=camera; Player=player; Level=level} = gameState
    let {Position=pos; Direction=dir} = player

    let mutable updated_pos =
        if UpPressed then
            pos + (s * dir)
        elif DownPressed then
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
        if RightPressed then
            Vec2.rotate (-sa) dir 
        elif LeftPressed then
            Vec2.rotate (sa) dir
        else
            dir

    let updated_camera =
        if RightPressed then
            Vec2.rotate (-sa) camera 
        elif LeftPressed then
            Vec2.rotate (sa) camera
        else
            camera

    { gameState with
        Player={gameState.Player with
                    Position = updated_pos
                    Direction = updated_dir}
        CameraPlane=updated_camera
        Ticks = t}
    
let overhead (gfx:Graphics2d) off intersections updatedState =

    let {Player=player;CameraPlane=camera;Level=level} = updatedState
    let {Position=pos;Direction=dir} = player
    
    intersections |> Seq.iter (fun (_, ray, (dist, _)) -> 
        let m = (dist * ray) + pos
        gfx.strokeLine (pos + off) (m + off) "white"
    )
    
    level.Map
    |> Seq.iter (fun wall -> gfx.strokeLine (wall.Start + off) (wall.End + off) "white")

    gfx.strokeText {x=8.; y=16.} (sprintf "%A" pos)
    gfx.strokeText {x=8.; y=32.} (sprintf "%A" dir)
    gfx.strokeText {x=8.; y=48.} (sprintf "%A" camera)
    
let raycast (gfx:Graphics2d) off intersections state =
    
    let {Player=player;CameraPlane=camera;Level=level} = state
    let {Position=pos;Direction=dir} = player
    
    gfx.fillRect off {x=Width;y=Height/2.} "rgb(48,48,48)" // ceiling
    gfx.fillRect (off + {x=0.; y=Height/2.}) {x=Width;y=Height/2.} "rgb(64,64,64)" // floor
    gfx.strokeRect off {x=Width;y=Height} "white" // border
    
    let height d = 1./d * WallHeight
    
    let w = Width / float(NumRays)

    intersections |> Seq.iter (fun (i, ray, (dist, norm)) -> 
        let m = (dist * ray) + pos
        let x = m - pos
        let v = Vec2.normalize camera
        let proj = (Vec2.dot x v) * v
        let rej = Vec2.mag (x - proj)
        let h = height rej

        let mutable c = 128
        let light_dir = Vec2.normalize {x = -1.;y = -2.}
        let b = (Vec2.dot norm light_dir)
        if (b > 0.) then c <- c + int(b * 128.) else c <- c + int(b * -128.)

        let clr = sprintf "rgb(%i,%i,%i)" c c c
        gfx.fillRect {x=0. + float(i) * w; y=0. + Height/2. - h} {x=w;y=h*2.} clr
    )
    
    ()
    
let render (gfx:Graphics2d) state =
    gfx.fillRect {x=0.;y=0.} {x=800.;y=600.} "#263545"
    
    let intersections =
        let {Player=player;CameraPlane=camera;Level=level} = state
        let {Position=pos;Direction=dir} = player
        
        [for i in 0..NumRays do yield (i, float(i)/float(NumRays))]
        |> Seq.map (fun (i, t) -> (i, ((1.0 - t) * (dir - camera)) + (t * (dir+camera))))
        |> Seq.map (fun (i, ray) -> (i, ray, level.intersectLevel pos ray))
    
    raycast gfx {x=0.; y=0.} intersections state
    
    overhead gfx {x=575.; y=25.} intersections state

let rec gameLoop (gfx:Graphics2d) t gameState =
    let updatedState = update t gameState

    render gfx updatedState

    window.requestAnimationFrame(fun t -> (gameLoop gfx t updatedState)) |> ignore

let canvas = document.querySelector(".view") :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
let gfx = new Graphics2d(ctx)

initKeyboard ()
gameLoop gfx 0. initState

