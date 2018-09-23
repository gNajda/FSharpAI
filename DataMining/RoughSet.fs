namespace DataMining

module RoughSet =
    open System

    type Decision = string
    type Attributes = string list
    type Rule = int * Attributes * Decision
    type ApproximationType = Lower | Upper | Boundary

    let private fst3 (a,_,_) = a
    let private snd3 (_,b,_) = b
    let private thd3 (_,_,c) = c

    type RuleTable(rawData: string seq seq) =
        do Seq.forall (fun (record:string seq) -> Seq.length record = Seq.length (Seq.item 0 rawData)) (List.ofSeq rawData) 
            |> fun i -> 
                match i with
                | false -> raise (ArgumentException("Not all records have the same number of columns"))
                | _ -> ()
        
        let convertToDataTable rawData = 
            Seq.take ((Seq.length rawData) - 1) rawData
            |> List.ofSeq
            |> List.map (fun row -> List.ofSeq row)
            |> List.mapi (fun i (row: string list) -> Rule(i ,(List.take ((Seq.length row) - 1) (List.ofSeq row)), (Seq.last row)))

        let attributeNames = Seq.item 0 rawData |> List.ofSeq
        let rules = convertToDataTable rawData

        member this.getAttribute(ruleIndex, attributeIndex) = Seq.item ruleIndex rules |> snd3 |> Seq.item attributeIndex
        member this.getRule(ruleIndex) = Seq.item ruleIndex rules
        member this.AttributeCount = Seq.length attributeNames
        member this.RuleCount = Seq.length rules
        member this.map (filter: Rule -> Rule) = List.map filter rules
        member this.AttributeNames = attributeNames

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

    let private getSpecifiedRules (initialRules:RuleTable) (attIndexes:int list) = 
        if List.max attIndexes > (initialRules.AttributeCount) && List.min attIndexes < 0
            then raise (ArgumentException("Attribute list contains incorrect column index"))
        else
            initialRules.map (fun rule -> 
            fst3 rule, 
            snd3 rule 
                |> List.mapi (fun i attr -> i,attr) 
                |> List.filter (fun (i, attr) -> List.contains i attIndexes) 
                |> List.map (fun (i, attr) -> attr), 
            thd3 rule)

    let calculateApproximation (data:RuleTable) (attIndexes:int list) (appType:ApproximationType) =
        getSpecifiedRules data attIndexes
        |> (fun rules -> 
                match appType with
                | Lower -> filterOutRules [] rules (fun rule -> isRuleConsistent rule rules)
                | Upper -> rules
                | Boundary -> filterOutRules [] rules (fun rule -> not (isRuleConsistent rule rules)))
        |> List.groupBy (fun (_,_,c) -> c)

    let getDistinctAttributes (ruleTable:RuleTable) (attIndexes:int list) =
        getSpecifiedRules ruleTable attIndexes
        |> List.map (fun (_,b,_) -> b)
        |> List.collect (fun row -> [for _i = 0 to row.Length - 1 do yield _i, row.[_i]])
        |> List.groupBy (fun (i,_) -> i)
        |> List.map (fun x -> List.map (fun y -> snd y) (snd x))
        |> List.map (fun x -> List.distinct (List.sort x))

    let calculateQualityOfApproximation (ruleTable:RuleTable) (attIndexes:int list) = 
        let rules = calculateApproximation ruleTable attIndexes ApproximationType.Lower
        let attCombination = getDistinctAttributes ruleTable attIndexes

        let left = List.sumBy ( fun (x:Decision*Rule list) -> (snd x).Length) rules |> float
        let right = List.fold (fun state (x:string list) -> state * x.Length) 1 attCombination |> float

        System.Math.Round(left/right, 3)

    let calculatePrecisionOfApproximation (ruleTable:RuleTable) (attIndexes:int list) = 
        let rules = getSpecifiedRules ruleTable attIndexes
        let lower = filterOutRules [] rules (fun rule -> isRuleConsistent rule rules)

        let left = lower.Length |> float
        let right = rules.Length |> float

        System.Math.Round(left/right, 3)