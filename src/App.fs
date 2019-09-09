module App

open Fable
open Fable.Core
open Fable.Import.Browser
open Math
open Model
open Level
open Graphics2d
open Keyboard

let NumRays = 200
let Width = 800.
let Height = 600.
let WallHeight = Height * 0.75
let CeilingColor = "#87cefa"
let FloorColor = "#567d46"

let brick = U3.Case1 (document.querySelector("#brick") :?> HTMLImageElement)

let initState:Model.GameState = {
    Ticks = 0.
    Player = {
        Position = { x = 6.5; y = 15.5 }
        Direction = Vec2.normalize { x = 0.7; y = -0.7 }
    }
    CameraPlane = Vec2.perp (0.33 * (Vec2.normalize { x = 0.7; y = -0.7  }))
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
    
    gfx.fillRect {x=0.;y=0.} {x=Width;y=80.} "rgb(0,0,0,0.5)"

    let {Player=player;CameraPlane=camera;Level=level} = updatedState
    let {Position=pos;Direction=dir} = player
    
    intersections |> Seq.iter (fun (_, ray, (dist, _, _)) -> 
        let m = (dist * ray) + pos
        gfx.strokeLine (pos + off) (m + off) "white"
    )
    
    level.Map
    |> Seq.iter (fun wall -> gfx.strokeLine (wall.Start + off) (wall.End + off) "white")

    gfx.strokeText {x=8.; y=16.} (sprintf "%A" pos)
    gfx.strokeText {x=8.; y=32.} (sprintf "%A" dir)
    gfx.strokeText {x=8.; y=48.} (sprintf "%A" camera)
    
let renderLevel (gfx:Graphics2d) off intersections state =
    
    let {Player=player;CameraPlane=camera} = state
    let {Position=pos} = player
    
    gfx.fillRect off {x=Width;y=Height/2.} CeilingColor // ceiling
    gfx.fillRect (off + {x=0.; y=Height/2.}) {x=Width;y=Height/2.} FloorColor // floor
    
    let height d = 1./d * WallHeight
    
    let w = Width / float(NumRays)

    intersections |> Seq.iter (fun (i, ray, (dist, norm, t)) ->
        
        //// Project the ray-wall intersection onto the camera plane and get distance
        // If we just use distance from intersection to player, we get a fish-eye effect
        let m = (dist * ray) + pos
        let x = m - pos
        let v = Vec2.normalize camera
        let proj = (Vec2.dot x v) * v
        let rej = Vec2.mag (x - proj)
        let h = height rej
        
        // Crude directional light calculations for now
        let light_dir = Vec2.normalize {x = 1.;y = 2.}
        let dot = Vec2.dot norm light_dir
        let c = if (abs(dot) > 0.5) then 0.5 else 0.
        console.log (Vec2.dot norm light_dir)
        let clr = sprintf "rgb(0,0,0,%f)" c
        
        // Compute texture coordinates
        let texture_width = 300.
        let texture_height = 300.
        let sx = (t - floor(t)) * (texture_width - 1.)
        let sy = 0.
        let sWidth = 1.
        let sHeight = texture_height
        let dx = 0. + float(i) * w
        let dy = 0. + Height/2. - h
        let dWidth = w
        let dHeight = h * 2.
        
        // draw vertical wall chunk
        gfx.drawImage brick sx sy sWidth sHeight dx dy dWidth dHeight
        
        // draw darker rect in front of vertical wall chunk to shade it
        gfx.fillRect {x=0. + float(i) * w; y=0. + Height/2. - h} {x=w;y=h*2.} clr
    )
    
    gfx.strokeRect off {x=Width;y=Height} "white" // border
    
let render (gfx:Graphics2d) state =
    gfx.fillRect {x=0.;y=0.} {x=800.;y=600.} "#263545"
    
    let intersections =
        let {Player=player;CameraPlane=camera;Level=level} = state
        let {Position=pos;Direction=dir} = player
        
        [for i in 0..NumRays-1 do yield (i, float(i)/float(NumRays))]
        |> Seq.map (fun (i, t) -> (i, ((1.0 - t) * (dir - camera)) + (t * (dir+camera))))
        |> Seq.map (fun (i, ray) -> (i, ray, level.intersect pos ray))
    
    renderLevel gfx {x=0.; y=0.} intersections state
    
    overhead gfx {x=750.; y=16.} intersections state

let rec gameLoop (gfx:Graphics2d) t gameState =
    let updatedState = update t gameState

    render gfx updatedState

    window.requestAnimationFrame(fun t -> (gameLoop gfx t updatedState)) |> ignore

let canvas = document.querySelector(".view") :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
let gfx = new Graphics2d(ctx)

initKeyboard ()
gameLoop gfx 0. initState

