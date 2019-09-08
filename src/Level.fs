module Level

open Math

type Wall = { Start: Vec2; End: Vec2 }
            static member create (x1,y1) (x2,y2) = { Start = { x = x1; y = y1 }; End = {x = x2; y = y2 } }
            
type Level() =
    member this.Map = [
        Wall.create (38.4,0.) (0.,0.)
        Wall.create (0.,0.) (0.,12.8) 
        Wall.create (0.,12.8) (0.,25.6)
        Wall.create (0.,25.6) (25.6,25.6)
        Wall.create (25.6,25.6) (25.6,12.8)
        Wall.create (25.6,12.8) (38.4,12.8)
        Wall.create (38.4,12.8) (38.4,0.)
        
        Wall.create (9.6, 6.4) (12.8,6.4)
        Wall.create (12.8,6.4) (12.8,9.6)
        Wall.create (12.8,9.6) (9.6,9.6)
        Wall.create (9.6,9.6) (9.6,6.4)
        
        Wall.create (9.6, 12.4) (12.8,12.4)
        Wall.create (12.8,12.4) (12.8,15.6)
        Wall.create (12.8,15.6) (9.6,15.6)
        Wall.create (9.6,15.6) (9.6,12.4)
    ]
    
    member this.intersect pos ray =
        this.Map
        |> Seq.map (fun w ->
                        (w,Math.intersect w.Start (Vec2.perp (w.End-w.Start)) pos ray))
        |> Seq.map (fun (w,dist) ->
                        match dist with
                        | Some dist -> 
                            let v = (dist * ray) + pos
                            if pointOnLine w.Start w.End v then
                                (dist, // distance to wall
                                 Vec2.normalize (Vec2.perp (w.End-w.Start)), // vector perpendicular to wall
                                 (Vec2.mag (v - w.Start))
                                )
                            else
                                (1000., {x=0.;y=0.}, 0.0)
                        | None -> (1000., {x=0.;y=0.}, 0.0))
        |> Seq.filter (fun (d,_,_) -> d >= 0.)
        |> Seq.minBy (fun (d,_,_) -> d)

