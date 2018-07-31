namespace DataMining

module RoughSet =
    open Data
    open System

    type ApproximationType = Lower | Upper | Boundary
    type Decision = string
    type Attributes = string list
    type Rule = int * Attributes * Decision

    let private fst3 (a,_,_) = a
    let private snd3 (_,b,_) = b
    let private thd3 (_,_,c) = c

    let private isRuleConsistent rule allRules = 
        not (List.exists (fun row -> 
        snd3 rule = snd3 row 
        && thd3 rule <> thd3 row) allRules)
    
    let rec private filterOutRules (state:Rule list) (inputRules:Rule list) (filter:Rule -> bool)=
        match inputRules with
        | currentRule::otherRules -> 
            match filter currentRule with 
                | true -> filterOutRules (currentRule::state) otherRules filter
                | false -> filterOutRules state otherRules filter
        | [] -> state

    let private getSpecifiedRules (initialRules:SimpleDataTable) (attIndexes:int list) = 
        let pickAttFromRow (row:string list) = List.take (row.Length - 1) row 
        let pickDecFromRow (row:string list) = List.last row 

        if List.max attIndexes >= (initialRules.ColumnCount - 1) 
            then raise (ArgumentException("Attribute list contains incorrect column index"))
        else
            let attAndDecIndexes = attIndexes @ [initialRules.ColumnCount - 1]
            [ for _i = 0 to initialRules.RowCount - 1 
                do yield _i, List.mapi (fun i el -> i, el) (List.ofSeq initialRules.[_i])
                    |> List.filter (fun (i, el) -> List.contains i attAndDecIndexes)
                    |> List.map (fun (i, el) -> el)]
                |> List.map (fun row -> fst row, pickAttFromRow (snd row), pickDecFromRow (snd row))

    let calculateApproximations (data:SimpleDataTable) (attIndexes:int list) (appType:ApproximationType) =
        getSpecifiedRules data attIndexes
        |> (fun rules -> 
                match appType with
                | Lower -> filterOutRules [] rules (fun rule -> isRuleConsistent rule rules)
                | Upper -> rules
                | Boundary -> filterOutRules [] rules (fun rule -> not (isRuleConsistent rule rules)))
        |> List.groupBy (fun (_,_,c) -> c)

    let getAttributeCombinations (data:SimpleDataTable) (attIndexes:int list) =
        getSpecifiedRules data attIndexes
        |> List.map (fun (_,b,_) -> b)
        |> List.collect (fun row -> [for _i = 0 to row.Length - 1 do yield _i, row.[_i]])
        |> List.groupBy (fun (i,_) -> i)
        |> List.map (fun x -> List.map (fun y -> snd y) (snd x))
        |> List.map (fun x -> List.distinct x)

    let rec test (state:string list) (attributes:string list list) =
        match attributes with
        | head::tail -> if tail.Length = 0 then 0 else 0