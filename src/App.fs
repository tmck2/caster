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
        Position = { x = 13.; y = 11. }
        Direction = { x = 0.; y = -1. }
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

module OverviewRenderer =

    let (CellSize:float) = 16.
    let (PlayerSize:float) = CellSize / 1.25

    let renderGrid (ctx:CanvasRenderingContext2D) offset level =
        for y in [0..level.Height-1] do
            for x in [0..level.Width-1] do
                let fillStyle =
                    match level.Map.[y].[x] with
                    | Solid _ -> "#ffffff"
                    | _ -> "#263545"
                ctx.fillStyle <- !^fillStyle
                let v = (CellSize * ({x=float(x); y=float(y)})) + offset
                ctx.fillRect (v.x, v.y, CellSize, CellSize)

    let renderPlayer (ctx:CanvasRenderingContext2D) (offset:Vec2) gameState =
        let { Player=player; CameraPlane=camera } = gameState
        let { Position=pos } = player

        ctx.fillStyle <- !^"rgb(200,0,0)"
        let pv = (CellSize * player.Position) + offset
        ctx.fillRect (pv.x-PlayerSize/2., pv.y-PlayerSize/2., PlayerSize, PlayerSize)

        ctx.strokeStyle <- !^"rgb(0,200,0)"
        ctx.beginPath()
        let f1 = offset + (CellSize * player.Position) + (20. * player.Direction)
        let f2 = offset + (CellSize * player.Position)
        ctx.moveTo(f1.x, f1.y)
        ctx.lineTo(f2.x, f2.y)
        ctx.stroke()

        let c1 = 150. * (player.Direction - camera) + (CellSize * player.Position) + offset
        let c2 = 150. * (player.Direction + camera) + (CellSize * player.Position) + offset
        ctx.beginPath()
        ctx.moveTo(f2.x, f2.y)
        ctx.lineTo(c1.x, c1.y)
        ctx.moveTo(c2.x, c2.y)
        ctx.lineTo(f2.x, f2.y)
        ctx.stroke()

    let render (ctx:CanvasRenderingContext2D) (offset:Vec2) gameState =
        let { Level = level } = gameState

        ctx.fillStyle <- !^"#263545"
        ctx.fillRect (0., 0., 640., 400.)

        renderGrid ctx offset level

        renderPlayer ctx offset gameState

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

let rec gameLoop (ctx:CanvasRenderingContext2D) t gameState =
    let updatedState = update t gameState

    ctx.fillStyle <- !^"#263545"
    ctx.fillRect (0.,0.,320.,200.)
    OverviewRenderer.render ctx {x=0.; y=0.} updatedState

    window.requestAnimationFrame(fun t -> (gameLoop ctx t updatedState)) |> ignore

let level = Level.loadLevel mapData

let canvas = document.querySelector(".view") :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()

Keyboard.initKeyboard ()
gameLoop ctx 0. initState

