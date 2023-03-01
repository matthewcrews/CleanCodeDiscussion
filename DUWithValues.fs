module CleanCode.DUWithValues

type Shape =
| Circle of radius : float32
| Square of side : float32
| Rectangle of length : float32 * width : float32
| Triangle of length : float32 * width : float32

module Shape =
    let area = function
        | Circle r -> r * r * Constants.pi
        | Square s -> s * s
        | Rectangle (l, w) -> l * w
        | Triangle (l, w) -> l * w * 0.5f

    let cornerCount = function
        | Circle _ -> 0.0f
        | Square _ -> 4.0f
        | Rectangle _ -> 4.0f
        | Triangle _ -> 3.0f

let shapes =
    [|
        for shape in Inheritance.shapes do
            match shape with
            | :? Inheritance.Circle as c ->
                Circle c.Radius
            | :? Inheritance.Square as s ->
                Square s.Length
            | :? Inheritance.Rectangle as r ->
                Rectangle (r.Width, r.Height)
            | :? Inheritance.Triangle as t ->
                Triangle (t.Width, t.Height)
            | _ ->
                failwith "Unknown subtype"
    |]