namespace CleanCode.Benchmarks

open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open CleanCode

[<HardwareCounters(HardwareCounter.TotalCycles,
                   HardwareCounter.BranchMispredictions)>]
[<DisassemblyDiagnoser(filters=[||])>]
type Benchmarks () =

    let duShapes = DU.shapes |> Array.copy

    [<Benchmark>]
    member _.Inheritance () =

        let mutable acc = 0.0f

        for shape in Inheritance.shapes do
            acc <- acc + (shape.Area() / (1.0f + shape.CornerCount()))

        acc

    [<Benchmark>]
    member _.Interface () =

        let mutable acc = 0.0f

        for shape in Interface.shapes do
            acc <- acc + (shape.Area() / (1.0f + shape.CornerCount()))

        acc

    [<Benchmark>]
    member _.DU () =

        let mutable acc = 0.0f

        for shape in DU.shapes do
            acc <- acc + (DU.Shape.area shape / (1.0f + DU.Shape.cornerCount shape))

        acc

    [<Benchmark>]
    member _.AltDULoop () =

        let circleCoef = Constants.pi / (1.0f + DU.Circle.cornerCount)
        let squareCoef = 1.0f / (1.0f + DU.Square.cornerCount)
        let rectangleCoef = 1.0f / (1.0f + DU.Rectangle.cornerCount)
        let triangleCoef = 0.5f / (1.0f + DU.Triangle.cornerCount)

        let mutable acc = 0.0f

        for shape in duShapes do
            acc <- acc +
                   match shape with
                   | DU.Shape.Circle circle ->
                       circleCoef * (DU.Circle.area circle)
                   | DU.Shape.Square square ->
                       squareCoef * (DU.Square.area square)
                   | DU.Shape.Rectangle rectangle ->
                       rectangleCoef * (DU.Rectangle.area rectangle)
                   | DU.Shape.Triangle triangle ->
                       triangleCoef * (DU.Triangle.area triangle)

        acc

    [<Benchmark>]
    member _.StructDU () =

        let circleCoef = Constants.pi / (1.0f + StructDU.Circle.cornerCount)
        let squareCoef = 1.0f / (1.0f + StructDU.Square.cornerCount)
        let rectangleCoef = 1.0f / (1.0f + StructDU.Rectangle.cornerCount)
        let triangleCoef = 0.5f / (1.0f + StructDU.Triangle.cornerCount)

        let mutable acc = 0.0f

        for shape in StructDU.shapes do
            acc <- acc +
                   match shape with
                   | StructDU.Shape.Circle circle ->
                       circleCoef * (StructDU.Circle.area circle)
                   | StructDU.Shape.Square square ->
                       squareCoef * (StructDU.Square.area square)
                   | StructDU.Shape.Rectangle rectangle ->
                       rectangleCoef * (StructDU.Rectangle.area rectangle)
                   | StructDU.Shape.Triangle triangle ->
                       triangleCoef * (StructDU.Triangle.area triangle)

        acc

    [<Benchmark>]
    member _.CompactDU () =

        let circleCoef = Constants.pi / (1.0f + CompactDU.Circle.cornerCount)
        let squareCoef = 1.0f / (1.0f + CompactDU.Square.cornerCount)
        let rectangleCoef = 1.0f / (1.0f + CompactDU.Rectangle.cornerCount)
        let triangleCoef = 0.5f / (1.0f + CompactDU.Triangle.cornerCount)

        let mutable acc = 0.0f

        for shape in CompactDU.shapes do
            let measures = shape.Measures
            acc <- acc +
                   match shape.Type with
                   | CompactDU.ShapeType.Circle ->
                       circleCoef * (measures.Measure1 * measures.Measure1)
                   | CompactDU.ShapeType.Square ->
                       squareCoef * (measures.Measure1 * measures.Measure1)
                   | CompactDU.ShapeType.Rectangle ->
                       rectangleCoef * (measures.Measure1 * measures.Measure2)
                   | CompactDU.ShapeType.Triangle ->
                       triangleCoef * (0.5f * measures.Measure1 * measures.Measure2)

        acc

    [<Benchmark>]
    member _.AltCompactDULoop () =

        let circleCoef = Constants.pi / (1.0f + CompactDU.Circle.cornerCount)
        let squareCoef = 1.0f / (1.0f + CompactDU.Square.cornerCount)
        let rectangleCoef = 1.0f / (1.0f + CompactDU.Rectangle.cornerCount)
        let triangleCoef = 0.5f / (1.0f + CompactDU.Triangle.cornerCount)

        let coefs =
            [|
                circleCoef
                squareCoef
                rectangleCoef
                triangleCoef
            |]

        let mutable acc = 0.0f

        for shape in CompactDU.shapes do
            let shapeType = int shape.Type
            let measures = shape.Measures
            acc <- acc + coefs[shapeType] * measures.Measure1 * measures.Measure2

        acc

    [<Benchmark(Baseline = true)>]
    member _.SeparateShapes () =

        let circleCoef = Constants.pi / (1.0f + SeparateShapes.Circle.cornerCount)
        let squareCoef = 1.0f / (1.0f + SeparateShapes.Square.cornerCount)
        let rectangleCoef = 1.0f / (1.0f + SeparateShapes.Rectangle.cornerCount)
        let triangleCoef = 0.5f / (1.0f + SeparateShapes.Triangle.cornerCount)

        let mutable acc = 0.0f
        let shapes = SeparateShapes.shapes

        for circle in shapes.Circles do
            acc <- acc + circleCoef * (SeparateShapes.Circle.area circle)

        for square in shapes.Squares do
            acc <- acc + squareCoef * (SeparateShapes.Square.area square)

        for rectangle in shapes.Rectangles do
            acc <- acc + rectangleCoef * (SeparateShapes.Rectangle.area rectangle)

        for triangle in shapes.Triangles do
            acc <- acc + triangleCoef * (SeparateShapes.Triangle.area triangle)

        acc
