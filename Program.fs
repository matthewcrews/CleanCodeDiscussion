open BenchmarkDotNet.Running
open CleanCode

[<EntryPoint>]
let main argv =
    let _ = BenchmarkRunner.Run<Benchmarks.Benchmarks>()
    1
