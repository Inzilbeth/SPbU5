namespace Interpolation

type public Interpolator() =

    member public Interpolator.getValues (a : double) (b : double) (m : double) (f : IFunction) =
        let rec getValuesLoop nodes values (j : int) =
            match j with
            | j when (float (j)) < m -> let xj = a + (float (j)) * (b - a) / m
                                        getValuesLoop (xj :: nodes) ((f.compute(xj)) :: values) (j + 1)

            | _ -> List.zip nodes values

        getValuesLoop [] [] 0

    member public Interpolator.prepareTable table (x : double) n =
        List.sortBy (fun (node, value) -> abs (node - x)) table |> List.take (n + 1)

    member public Interpolator.interpolateLagrange table x =
        let rec interpolationLoop list sum =
            let calculateDenominator kthNode =
                List.fold (fun acc (node, value) -> if node = kthNode then acc else acc * (kthNode - node)) 1.0 table

            let calculateNumerator kthNode =
                List.fold (fun acc (node, value) -> if node = kthNode then acc else acc * (x - node)) 1.0 table

            match list with
            | (node, value) :: tail -> interpolationLoop tail (sum + (calculateNumerator node) / (calculateDenominator node) * value)
            | [] -> sum

        interpolationLoop table 0.0

    member public Interpolator.interpolateNewton table x =
        let rec interpolationLoop list multiplier count previous sum =
            let rec calculateDifference list =
                match list with
                | (node, value) :: tail when tail = [] -> value
                | (node, value) :: tail -> (calculateDifference tail - calculateDifference (List.take (list.Length - 1) list)) / ((List.item (list.Length - 1) list |> fst) - node)

            match list with
            | (node, value) :: tail -> if count = 0 then interpolationLoop tail 1.0 1 node (sum + value)
                                       else let newMultiplier = (multiplier * (x - previous))
                                            interpolationLoop tail newMultiplier (count + 1) node (sum + (calculateDifference (List.take (count + 1) table)) * newMultiplier)
            | [] -> sum

        interpolationLoop table 1.0 0 1.0 0.0