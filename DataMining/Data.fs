namespace DataMining

module Data =
    open System

    type SimpleDataTable(rawData: string seq seq) =
        do Seq.forall (fun (record:string seq) -> Seq.length record = Seq.length (Seq.item 0 rawData)) (List.ofSeq rawData) 
            |> fun i -> 
                match i with
                | false -> raise (ArgumentException("Not all records have the same number of columns"))
                | _ -> ()

        let data = rawData

        member this.Item with get(columnIndex, rowIndex) = Seq.item rowIndex data |> Seq.item columnIndex
        member this.Item with get(rowIndex) = Seq.item rowIndex data
        member this.ColumnCount = 
            Seq.item 0 data 
            |> Seq.length

        member this.RowCount = Seq.length data