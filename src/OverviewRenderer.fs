module OverviewRenderer

open Math
open Graphics2d
open Model
open Level

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

