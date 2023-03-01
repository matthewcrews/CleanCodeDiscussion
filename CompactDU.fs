module CleanCode.CompactDU

open Microsoft.FSharp.Core

[<Struct>]
type Measures =
    {
        Measure1: float32
        Measure2: float32
    }

[<Struct>]
type ShapeType =
    | Circle = 0
    | Square = 1
    | Rectangle = 2
    | Triangle = 3

module Circle =
    let cornerCount = 0.0f

module Square =
    let cornerCount = 4.0f

module Rectangle =
    let cornerCount = 4.0f

module Triangle =
    let cornerCount = 3.0f

[<Struct>]
type Shape =
    {
        Type: ShapeType
        Measures: Measures
    }

module Shape =

    module Circle =

        let create (radius: float32) =
            let measures = {
                Measure1 = radius
                Measure2 = radius
            }
            {
                Type = ShapeType.Circle
                Measures = measures
            }

    module Square =

        let create (length: float32) =
            let measures = {
                Measure1 = length
                Measure2 = length
            }
            {
                Type = ShapeType.Square
                Measures = measures
            }

    module Rectangle =

        let create (width: float32) (height: float32) =
            let measures = {
                Measure1 = width
                Measure2 = height
            }
            {
                Type = ShapeType.Rectangle
                Measures = measures
            }

    module Triangle =

        let create (width: float32) (height: float32) =
            let measures = {
                Measure1 = width
                Measure2 = height
            }
            {
                Type = ShapeType.Triangle
                Measures = measures
            }


let shapes =
    [| for shape in Inheritance.shapes do
        match shape with
        | :? Inheritance.Circle as c ->
            Shape.Circle.create c.Radius
        | :? Inheritance.Square as s ->
            Shape.Square.create s.Length
        | :? Inheritance.Rectangle as r ->
            Shape.Rectangle.create r.Width r.Height
        | :? Inheritance.Triangle as t ->
            Shape.Triangle.create t.Width t.Height
        | _ ->
            failwith "Unknown subtype"
    |]
