module CleanCode.StructDU

[<Struct>]
type Circle =
    {
        Radius: float32
    }

module Circle =

    let create radius =
        { Radius = radius }

    let area c =
        c.Radius * c.Radius * Constants.pi

    let cornerCount = 0.0f

[<Struct>]
type Square =
    {
        Length: float32
    }

module Square =

    let create length =
        { Length = length }

    let area s =
        s.Length * s.Length

    let cornerCount = 4.0f

[<Struct>]
type Rectangle =
    {
        Width: float32
        Height: float32
    }

module Rectangle =

    let create width height =
        {
            Width = width
            Height = height
        }

    let area r =
        r.Width * r.Height

    let cornerCount = 4.0f

[<Struct>]
type Triangle =
    {
        Width: float32
        Height: float32
    }

module Triangle =

    let create width height =
        {
            Width = width
            Height = height
        }

    let area t =
        t.Width * t.Height * 0.5f

    let cornerCount = 3.0f

[<RequireQualifiedAccess>]
type Shape =
    | Circle of circle: Circle
    | Square of square: Square
    | Rectangle of rectangle: Rectangle
    | Triangle of triangle: Triangle

module Shape =

    let area = function
        | Shape.Circle c -> Circle.area c
        | Shape.Square s -> Square.area s
        | Shape.Rectangle r -> Rectangle.area r
        | Shape.Triangle t -> Triangle.area t

let shapes =
    [| for shape in Inheritance.shapes do
        match shape with
        | :? Inheritance.Circle as c ->
            Circle.create c.Radius
            |> Shape.Circle
        | :? Inheritance.Square as s ->
            Square.create s.Length
            |> Shape.Square
        | :? Inheritance.Rectangle as r ->
            Rectangle.create r.Width r.Height
            |> Shape.Rectangle
        | :? Inheritance.Triangle as t ->
            Triangle.create t.Width t.Height
            |> Shape.Triangle
        | _ ->
            failwith "Unknown subtype"
    |]
