open System
open System.IO
open DataMining
open Data

type Test = { Val1: string; Val2: string List }

[<EntryPoint>]
let main argv =

    #if DEBUG
    System.Linq.Enumerable.Count([]) |> ignore
    #endif

    let aa = { Val1 = "aa"; Val2 = ["test"] }
    let bb = aa.Val2.Head

    aa.Val2 = "ee" :: aa.Val2



    //let filePath = Path.Combine [|Directory.GetCurrentDirectory(); "TestData.txt"|]

    //let aa = new DataMining.Data.SimpleDataTable(File.ReadLines filePath |> fun rows -> seq {for row in rows -> Seq.ofArray (row.Split ",")})

    //let classes = RoughSet.calculateApproximation aa [0;1] RoughSet.ApproximationType.Lower

    //let attributes = RoughSet.calculatePrecisionOfApproximation aa [0;1]

    printfn "Hello World from F#!"
    0
