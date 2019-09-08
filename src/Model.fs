module Model

open Math
open Level

type Player = {
    Position: Vec2
    Direction: Vec2
}

type GameState = {
    Ticks: float
    Player: Player
    CameraPlane: Vec2
    Level: Level
}

