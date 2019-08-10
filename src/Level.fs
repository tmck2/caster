module Level

type Color =
    | Color of int * int * int

type Block =
    | Solid of Color
    | Empty

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

