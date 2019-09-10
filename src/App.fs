module App

open Fable
open Fable.Core
open Fable.Import.Browser
open Math
open Model
open Level
open Graphics2d
open Keyboard

let NumRays = 320
let Width = 320.
let Height = 200.
let WallHeight = Height
let CeilingColor = "#87cefa"
let FloorColor = "#567d46"

// TODO: move textures into own type and associate them with walls so that we can have different textures per wall
let brick = U3.Case1 (document.querySelector("#brick") :?> HTMLImageElement)
let TextureWidth = 300.
let TextureHeight = 300.

let initState:Model.GameState = {
    Ticks = 0.
    Player = {
        Position = { x = 6.5; y = 15.5 }
        Direction = Vec2.normalize { x = 0.7; y = -0.7 }
    }
    CameraPlane = Vec2.perp (0.33 * (Vec2.normalize { x = 0.7; y = -0.7  }))
    Level = new Level()
    ShowDebugConsole = false
}

let update t gameState =
    let s = (t-gameState.Ticks)/200.
    let sa = s/2.
    let {CameraPlane=camera; Player=player; Level=level} = gameState
    let {Position=pos; Direction=dir} = player

    let mutable updated_pos =
        if Keys.[UpKey] then
            pos + (s * dir)
        elif Keys.[DownKey] then
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
        if Keys.[RightKey] then
            Vec2.rotate (-sa) dir 
        elif Keys.[LeftKey] then
            Vec2.rotate (sa) dir
        else
            dir

    let updated_camera =
        if Keys.[RightKey] then
            Vec2.rotate (-sa) camera 
        elif Keys.[LeftKey] then
            Vec2.rotate (sa) camera
        else
            camera
            
    let showDebugConsole =
        if Keys.[CKey] then
            not gameState.ShowDebugConsole
        else
            gameState.ShowDebugConsole
    Keys.[CKey] <- false
            
    { gameState with
        ShowDebugConsole = showDebugConsole
        Player={gameState.Player with
                    Position = updated_pos
                    Direction = updated_dir}
        CameraPlane=updated_camera
        Ticks = t}
    
let overheadMap (gfx:Graphics2d) off intersections updatedState =

    let {Player=player;Level=level} = updatedState
    let {Position=pos} = player
    
    intersections |> Seq.iter (fun (_, ray, (dist, _, _)) -> 
        let m = (dist * ray) + pos
        gfx.strokeLine (pos + off) (m + off) "white"
    )
    
    level.Map
    |> Seq.iter (fun wall -> gfx.strokeLine (wall.Start + off) (wall.End + off) "white")

    
let debugConsole (gfx:Graphics2d) off intersections state =
    
    let {Player=player;CameraPlane=camera} = state
    let {Position=pos;Direction=dir} = player
    
    gfx.fillRect {x=0.;y=0.} {x=Width;y=34.} "rgb(0,0,0,0.5)"
    
    overheadMap gfx {x=255.; y=4.} intersections state
    
    gfx.strokeText {x=4.; y=8.} (sprintf "pos %A" pos)
    gfx.strokeText {x=4.; y=18.} (sprintf "dir %A" dir)
    gfx.strokeText {x=4.; y=28.} (sprintf "cam %A" camera)
    
    
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
        let clr = if (abs(dot) > 0.5) then "rgb(0,0,0,0.5)" else "rgb(0,0,0,0)"
        
        // Compute texture coordinates
        let sx = (t - floor(t)) * (TextureWidth - 1.)
        let sy = 0.
        let sWidth = 1.
        let sHeight = TextureHeight
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
    
    if state.ShowDebugConsole then
        debugConsole gfx {x=255.; y=4.} intersections state

let rec gameLoop (gfx:Graphics2d) t gameState =
    let updatedState = update t gameState

    render gfx updatedState

    window.requestAnimationFrame(fun t -> (gameLoop gfx t updatedState)) |> ignore

let canvas = document.querySelector(".view") :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
let gfx = new Graphics2d(ctx)

initKeyboard ()
gameLoop gfx 0. initState

