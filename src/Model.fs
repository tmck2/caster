module Model

open Math
open Level

type Player = {
    Position: Vec2
    Direction: Vec2
}

type Wall = { Start: Vec2; End: Vec2 }
            static member createWall (x1,y1) (x2,y2) = { Start = { x = x1; y = y1 }; End = {x = x2; y = y2 } }

type Level = {
    Map: Wall list
}

type GameState = {
    Ticks: float
    Player: Player
    CameraPlane: Vec2
    Level: Level
}

