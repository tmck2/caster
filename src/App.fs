module App

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.Helpers.React
open Fable.Helpers.React.Props

type Vec2 = 
    { x: float; y: float }
    static member (*) (s:float, v:Vec2):Vec2 = { x = s*v.x; y = s*v.y }
    static member (+) (a:Vec2, b:Vec2):Vec2 = { x = a.x + b.x; y = a.y + b.y }

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
    Player = {
        Position = { x = 13.; y = 11. }
        Direction = { x = 0.; y = -1. }
    }
    CameraPlane = { x = 0.; y = 0.66 }
    Level = Level.loadLevel mapData
}

module OverviewRenderer =
    let (CellSize:int) = 8
    let (PlayerSize:int) = CellSize / 2

    let renderGrid (ctx:CanvasRenderingContext2D) (xo, yo) level =
        for y in [0..level.Height-1] do
            for x in [0..level.Width-1] do
                let fillStyle =
                    match level.Map.[y].[x] with
                    | Solid _ -> "#ffffff"
                    | _ -> "#263545"
                ctx.fillStyle <- !^fillStyle
                ctx.fillRect (
                    float(x * CellSize + xo),
                    float(y * CellSize + yo),
                    float(CellSize),
                    float(CellSize))

    let renderPlayer (ctx:CanvasRenderingContext2D) (xo, yo) player =
        ctx.fillStyle <- !^"rgb(200,0,0)"
        ctx.fillRect (
            float(int(player.Position.x) * CellSize + xo),
            float(int(player.Position.y) * CellSize + yo),
            float(PlayerSize), float(PlayerSize) )

    let render (ctx:CanvasRenderingContext2D) (xo, yo) gameState =
        let { Level = level; Player = player } = gameState

        renderGrid ctx (xo, yo) level

        renderPlayer ctx (xo, yo) player


let level = Level.loadLevel mapData

let canvas = document.querySelector(".view") :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()

ctx.fillStyle <- !^"#263545"
ctx.fillRect (0.,0.,320.,200.)

OverviewRenderer.render ctx (0,0) initState

