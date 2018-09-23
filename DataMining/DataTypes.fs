namespace DataMining

module DataTypes =
    type Cell = { Value: string; ColumnIndex: int; RowIndex: int }
    type Row = { Index: int; Cells: Cell seq}
    type Column = { Index : int; Name: string; Cells: Cell seq }

    let getRowsFromCells cells =
        List.map (fun x -> x.RowIndex) cells
        |> List.distinct
        |> List.map (fun x -> 
            { Index = x; 
            Cells = seq { yield! List.where (fun y -> y.RowIndex = x) cells 
            |> List.sortBy (fun y -> y.ColumnIndex) }})

    let filterColumnsFromCells columns cells =
        List.map (fun x -> x.ColumnIndex) cells
        |> List.distinct 
        |> (fun x -> List.where (fun y -> List.contains y.Index x) columns)
        |> List.map (fun x -> 
            { Index = x.Index; 
            Name = x.Name; 
            Cells = seq { yield! List.where (fun y -> y.ColumnIndex = x.Index) cells 
            |> List.sortBy (fun y -> y.RowIndex) }})

    type Table(cells: Cell list, columns: Column list, rows: Row list) =
        let cells = cells
        let columns = columns
        let rows = rows

        new() = Table([], [], [])

        member this.Item with get(columnIndex, rowIndex) = 
            List.tryFind (fun (x:Row) -> x.Index = rowIndex) rows
            |> (fun x -> x.Value.Cells)
            |> Seq.tryFind (fun (x:Cell) -> x.ColumnIndex = columnIndex)

        member this.ColumnCount = columns.Length
        member this.RowCount = rows.Length
        member this.Rows = rows
        member this.Cells = cells
        member this.Columns = columns

        static member extractByCell filter (table: Table) =
            let cells = table.Cells |> List.where filter
            let columns = filterColumnsFromCells table.Columns cells
            let rows = getRowsFromCells cells
            new Table(cells, columns, rows)

        static member distinctRows (table: Table) =
            let rows = List.distinctBy (fun (x: Row) -> Seq.map (fun y -> y.Value) x.Cells |> Seq.concat) table.Rows
            let cells = List.collect (fun (x: Row) -> x.Cells |> List.ofSeq) rows
            new Table(cells, table.Columns, rows)