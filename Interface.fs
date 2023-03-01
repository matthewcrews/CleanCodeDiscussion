module CleanCode.Interface

type IShape =
    abstract member Area: unit -> float32
    abstract member CornerCount: unit -> float32

type Circle (radius: float32) =
    interface IShape with
        member _.Area () = (radius * radius) * Constants.pi
        member _.CornerCount () = 0.0f

type Square (length: float32) =
    interface IShape with
        member _.Area () = length * length
        member _.CornerCount () = 4.0f

type Rectangle (width: float32, height: float32) =
    interface IShape with
        member _.Area () = width * height
        member _.CornerCount () = 4.0f

type Triangle (width: float32, height: float32) =
    interface IShape with
        member _.Area () = 0.5f * width * height
        member _.CornerCount () = 3.0f

let shapes =
    [| for shape in Inheritance.shapes do
        match shape with
        | :? Inheritance.Circle as c ->
            Circle c.Radius :> IShape
        | :? Inheritance.Square as s ->
            Square s.Length :> IShape
        | :? Inheritance.Rectangle as r ->
            Rectangle (r.Width, r.Height) :> IShape
        | :? Inheritance.Triangle as t ->
            Triangle (t.Width, t.Height) :> IShape
        | _ ->
            failwith "Unknown subtype"
    |]
