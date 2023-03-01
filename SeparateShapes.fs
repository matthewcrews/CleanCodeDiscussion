module CleanCode.SeparateShapes

open System.Collections.Generic

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

type Shapes =
    {
        Circles: Circle[]
        Squares: Square[]
        Rectangles: Rectangle[]
        Triangles: Triangle[]
    }

let shapes =
    let circles = Stack()
    let squares = Stack()
    let rectangles = Stack()
    let triangles = Stack()

    for shape in Inheritance.shapes do
        match shape with
        | :? Inheritance.Circle as c ->
            Circle.create c.Radius
            |> circles.Push
        | :? Inheritance.Square as s ->
            Square.create s.Length
            |> squares.Push
        | :? Inheritance.Rectangle as r ->
            Rectangle.create r.Width r.Height
            |> rectangles.Push
        | :? Inheritance.Triangle as t ->
            Triangle.create t.Width t.Height
            |> triangles.Push
        | _ ->
            failwith "Unknown subtype"

    {
        Circles = circles.ToArray()
        Squares = squares.ToArray()
        Rectangles = rectangles.ToArray()
        Triangles = triangles.ToArray()
    }
