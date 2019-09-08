module Math

type Vec2 = 
    { x: float; y: float }
    
    override this.ToString() =
        sprintf "(%.3f, % .3f)" this.x this.y;
    
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
    static member perp (v:Vec2) = {x = -v.y; y=v.x}

// intersect :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> float
// vector-plane intersection
// p0: point on plane
// n: plane normal
// l0: starting point of line
// l: direction of line
let intersect p0 n l0 l:float option =
    let denom = Vec2.dot l n
    if denom <= 0. then
        None
    else
        Some ((Vec2.dot (p0 - l0) n) / denom)

// pointOnLine :: Vec2 -> Vec2 -> Vec2 -> bool
// l0: line endpoint
// l1: opposite line endpoint
// p: point to test
let pointOnLine l0 l1 p =
    let lineLen = Vec2.mag (l1 - l0)
    let d1 = Vec2.mag (p - l0)
    let d2 = Vec2.mag (p - l1)
    let alpha = 0.0001

    d1+d2 >= lineLen-alpha && d1+d2 <= lineLen+alpha

// lineCircleCollision
// l0: line endpoint
// l1: opposite endpoint
// c: center of circle
// r: radius of circle
let lineCircleCollision l0 l1 c r =
    let l = l1 - l0
    let v = c - l0
    let k = (Vec2.dot v l) / ((Vec2.mag l)**2.)
    let i = l0 + (k * l)
    let d = Vec2.mag (c - i)
    d <= r && (pointOnLine l0 l1 i)

