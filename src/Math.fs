module Math

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

let intersect p0 l0 l n:Vec2 option =
    let denom = Vec2.dot l n
    if denom <= 0. then
        None
    else
        let d = (Vec2.dot (p0 - l0) n) / denom
        Some ((d * l) + l0)

