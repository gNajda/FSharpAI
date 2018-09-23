namespace DataMining.RoughSet

module RoughSet_Test =
    open DataMining.DataTypes

    type ApproximationType = Lower | Upper | Boundary

    let private filterAttributes (table: Table) (attIndexes:int list) =
        Table.extractByCell (fun x -> List.contains x.ColumnIndex attIndexes) table

    let getDistinctAttributes (table: Table) (attIndexes:int list) =
        filterAttributes table attIndexes
        |> Table.distinctRows