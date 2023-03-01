module CleanCode.Inheritance

[<AbstractClass>]
type Shape() =
    abstract member Area: unit -> float32
    abstract member CornerCount: unit -> float32

type Circle (radius: float32) =
    inherit Shape ()
    override c.Area () = (radius * radius) * Constants.pi
    override c.CornerCount () = 0.0f
    member _.Radius = radius

type Square (length: float32) =
    inherit Shape ()
    override c.Area () = length * length
    override c.CornerCount () = 4.0f
    member _.Length = length

type Rectangle (width: float32, height: float32) =
    inherit Shape ()
    override c.Area () = width * height
    override c.CornerCount () = 4.0f
    member _.Width = width
    member _.Height = height

type Triangle (width: float32, height: float32) =
    inherit Shape ()
    override c.Area () = 0.5f * width * height
    override c.CornerCount () = 3.0f
    member _.Width = width
    member _.Height = height

let rng = System.Random 123
let shapeCount = 1048576

let shapes =
    [| for _ in 1..shapeCount do

       let shapeType = rng.NextDouble()
       if shapeType < 0.25 then
           // printfn "Circle"
           let radius = 1.0f + 10.0f * (rng.NextSingle ())
           Circle radius
           :> Shape
       elif shapeType < 0.5 then
           // printfn "Square"
           let length = 1.0f + 10.0f * (rng.NextSingle())
           Square length
           :> Shape
       elif shapeType < 0.75 then
           // printfn "Rectangle"
           let width = 1.0f + 10.0f * (rng.NextSingle())
           let height = 1.0f + 10.0f * (rng.NextSingle())
           Rectangle (width, height)
           :> Shape
       else
           // printfn "Triangle"
           let width = 1.0f + 10.0f * (rng.NextSingle())
           let height = 1.0f + 10.0f * (rng.NextSingle())
           Triangle (width, height)
           :> Shape
    |]
