namespace DataMining

module RoughSet_Test2 =

    type Attribute = { Value: string; Name: string }
    type Rule = { Index: int; Attributes: Attribute list; Decision: string }
    type ApproximationType = Lower | Upper | Boundary

    let extractRulesByAttributes rules attributes =
        List.fold (fun (acc: Rule list) (elem: Rule) -> 
            let filterAttributes = 
                let filter att = List.contains att.Name attributes
                List.where filter
            
            filterAttributes elem.Attributes
            |> (fun newAtt ->
            { elem with Attributes = newAtt } :: acc)) [] rules

    let compareAndFilter condition rules =
        let excludeRule toExclude allRules = 
            List.where (fun x -> x.Index <> toExclude.Index) allRules
        
        List.fold (fun (acc: Rule list) (rule: Rule) -> 
            excludeRule rule rules
            |> condition rule
            |> function
                | true -> acc
                | false -> rule::acc) [] rules

    let calculateApproximation appType attr rules =
        let basicCondition = fun currentRule otherRules-> 
            List.exists (fun el -> currentRule.Attributes = el.Attributes || currentRule.Decision = el.Decision ) otherRules

        let getUniqueRules rules = compareAndFilter basicCondition rules

        let getNonUniqueRules rules = 
            let notCondition rule = basicCondition rule >> not
            compareAndFilter notCondition rules

        extractRulesByAttributes rules attr |>
        (fun rules -> 
            match appType with
            | Lower -> getUniqueRules rules
            | Upper -> rules
            | Boundary -> getNonUniqueRules rules)