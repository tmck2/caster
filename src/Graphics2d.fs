module Graphics2d

open Fable.Import.JS
open Fable.Core.JsInterop
open Fable.Import.Browser
open Math

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

    member this.strokeText (p:Vec2) (str:string) =
        ctx.fillStyle <- !^("white")
        ctx.font <- "8px sans-serif"
        ctx.fillText(str,p.x,p.y)
        
    member this.drawImage image sx sy sWidth sHeight dx dy dWidth dHeight =
        ctx.drawImage (image, sx, sy, sWidth, sHeight, dx, dy, dWidth, dHeight)
        
    
