module App

open Fable.Core
open Fable.Import.JS
open Fable.Core.JsInterop
open Fable.Import.Browser

type Vec2 = 
    { x: float; y: float }
    static member (*) (s:float, v:Vec2):Vec2 = { x = s*v.x; y = s*v.y }
    static member (+) (a:Vec2, b:Vec2):Vec2 = { x = a.x + b.x; y = a.y + b.y }
    static member (-) (a:Vec2, b:Vec2):Vec2 = { x = a.x - b.x; y = a.y - b.y }
    static member mag (v:Vec2) = sqrt(v.x*v.x+v.y*v.y)
    static member dot (v1:Vec2) (v2:Vec2) = v1.x*v2.x+v1.y*v2.y
    static member normalize (v:Vec2) =
        let d = Vec2.mag v
        1.0/d * v
    static member rotate (rads:float) (v:Vec2) =
        { v with 
            x = v.x * cos(rads) + v.y * sin(rads)
            y = -v.x * sin(rads) + v.y * cos(rads)
        }

type Color =
    | Color of int * int * int

type Block =
    | Solid of Color
    | Empty

type Player = {
    Position: Vec2
    Direction: Vec2
}

type Level =
    { Width: int; Height: int; Map: Block[][] }
    static member loadLevel (mapData:string list):Level =
        { 
            Width = mapData |> List.map (fun s -> s.Length) |> List.max
            Height = List.length mapData
            Map = [|
                for row in mapData do
                yield [|
                    for col in row do
                    match col with
                    | '1' -> yield Solid (Color (192,192,192))
                    | _ -> yield Empty
                |]
            |]
        }

type GameState = {
    Ticks: float
    Player: Player
    CameraPlane: Vec2
    Level: Level
}

let mapData = [
    "111111111111111111111111"
    "1......................1"
    "1......................1"
    "1......................1"
    "1.....1..........1.....1"
    "1......................1"
    "1......................1"
    "1......................1"
    "1.....1..........1.....1"
    "1......................1"
    "1......................1"
    "1......................1"
    "1.....1..........1.....1"
    "1......................1"
    "1......................1"
    "1......................1"
    "1.....1..........1.....1"
    "1......................1"
    "1......................1"
    "1......................1"
    "1.....1..........1.....1"
    "1......................1"
    "1......................1"
    "1......................1"
    "111111111111111111111111"
]

let initState = {
    Ticks = 0.
    Player = {
        Position = { x = 64.; y = 64. }
        Direction = { x = sqrt(2.); y = -sqrt(2.) }
    }
    CameraPlane = { x = 0.66; y = 0. }
    Level = Level.loadLevel mapData
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

type Graphics2d(ctx:CanvasRenderingContext2D) =
    let ctx = ctx

    member this.strokeRect (v1:Vec2) (dim:Vec2) (color:string) =
        ctx.strokeStyle <- !^color
        ctx.strokeRect (v1.x, v1.y, dim.x, dim.y)

    member this.fillRect (v1:Vec2) (dim:Vec2) (color:string) =
        ctx.fillStyle <- !^color
        ctx.fillRect (v1.x, v1.y, dim.x, dim.y)

    member this.strokeLine (v1:Vec2) (v2:Vec2) (color:string) =
        ctx.beginPath()
        ctx.strokeStyle <- !^color
        ctx.moveTo (v1.x, v1.y)
        ctx.lineTo (v2.x, v2.y)
        ctx.stroke()

    member this.fillCircle (c:Vec2) (r:float) (color:string) =
        ctx.beginPath()
        ctx.fillStyle <- !^color
        ctx.arc(c.x, c.y, r, 0., 2. * Math.PI)
        ctx.fill()

module OverviewRenderer =

    let (CellSize:float) = 16.
    let (PlayerSize:float) = CellSize / 1.5

    let cellToWorld offset x y = (CellSize * {x=float(x); y=float(y)}) + offset

    let renderGrid (gfx:Graphics2d) offset level =
        for y in [0..level.Height-1] do
            for x in [0..level.Width-1] do
                gfx.strokeRect (cellToWorld offset x y) {x=CellSize; y=CellSize} "rgb(200,200,200)"

    let renderLevel (gfx:Graphics2d) offset level =
        for y in [0..level.Height-1] do
            for x in [0..level.Width-1] do
                let fillStyle =
                    match level.Map.[y].[x] with
                    | Solid _ -> "#ffffff"
                    | _ -> "#263545"
                gfx.fillRect (cellToWorld offset x y) {x=CellSize; y=CellSize} fillStyle

    let renderPlayer (gfx:Graphics2d) (offset:Vec2) gameState =
        let { Player=player; CameraPlane=camera } = gameState
        let { Position=pos } = player

        let pv = (CellSize * player.Position) - (0.5 * {x=PlayerSize;y=PlayerSize}) + offset
        gfx.fillRect pv {x=PlayerSize;y=PlayerSize} "rgb(200,0,0)"

        let f1 = offset + (CellSize * player.Position) + (20. * player.Direction)
        let f2 = offset + (CellSize * player.Position)
        gfx.strokeLine f1 f2 "rgb(0,200,0)"

        let c1 = 150. * (player.Direction - camera) + (CellSize * player.Position) + offset
        let c2 = 150. * (player.Direction + camera) + (CellSize * player.Position) + offset
        gfx.strokeLine f2 c1 "rgb(0,200,0)"
        gfx.strokeLine c2 f2 "rgb(0,200,0)"

    let render (gfx:Graphics2d) (offset:Vec2) gameState =
        let { Level = level } = gameState

        gfx.fillRect {x=0.; y=0.} {x=640.; y=400.} "#263545"

        renderLevel gfx offset level

        renderGrid gfx offset level

        renderPlayer gfx offset gameState

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

let intersect p0 l0 l n:Vec2 option =
    let denom = Vec2.dot l n
    if denom <= 0. then
        None
    else
        let d = (Vec2.dot (p0 - l0) n) / denom
        Some ((d * l) + l0)

(*
    TODO: On intersections, y coordinates are upside down
*)

let test (gfx:Graphics2d) updatedState =
    gfx.fillRect {x=0.;y=0.} {x=640.;y=400.} "#263545"

    let {Player=player} = updatedState
    let {Position=p;Direction=r} = player
    let off = {x=200.;y=100.}
    let size = {x=128.;y=128.}

    let paintIntersection x =
        match x with
        | Some i -> gfx.fillCircle (i+off) 5. "red"
        | None -> ()

    intersect {x=0.;y=128.} p r {x=0.;y= 1.} |> paintIntersection
    intersect {x=128.;y=0.} p r {x=1.; y=0.} |> paintIntersection
    intersect {x=0.;y=0.} p r {x= -1.;y= 0.} |> paintIntersection
    intersect {x=0.;y=0.} p r {x= 0.;y= -1.} |> paintIntersection

    gfx.strokeRect off size "white"
    gfx.strokeLine (p + off) (600. * r + p + off) "white"

let rec gameLoop (gfx:Graphics2d) t gameState =
    let updatedState = update t gameState

    //OverviewRenderer.render gfx {x=0.; y=0.} updatedState
    test gfx updatedState

    window.requestAnimationFrame(fun t -> (gameLoop gfx t updatedState)) |> ignore

let level = Level.loadLevel mapData

let canvas = document.querySelector(".view") :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
let gfx = new Graphics2d(ctx)

Keyboard.initKeyboard ()
gameLoop gfx 0. initState

