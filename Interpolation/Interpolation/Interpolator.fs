namespace Interpolation

type public Interpolator() =

    member public Interpolator.getValues (a : double) (b : double) (m : double) (f : IFunction) =
        let rec getValuesLoop nodes values (j : int) =
            match j with
            | j when (float (j)) < m -> let xj = a + (float (j)) * (b - a) / m
                                        getValuesLoop (xj :: nodes) ((f.compute(xj)) :: values) (j + 1)

            | _ -> List.zip nodes values

        getValuesLoop [] [] 0

    member public Interpolator.prepareTable table x n =
        List.sortBy (fun (node, value) -> abs (node - x)) table |> List.take (n + 1)

    member public Interpolator.interpolateLagrange table x =
        let rec interpolationLoop list sum =
            let calculateDenominator kthNode =
                List.fold (fun acc (node, value) -> if node = kthNode then acc else acc * (kthNode - node)) 1.0 table

            let calculateNumerator kthNode =
                List.fold (fun acc (node, value) -> if node = kthNode then acc else acc * (x - node)) 1.0 table

            match list with
            | (node, value) :: tail -> interpolationLoop tail ((calculateNumerator node) / (calculateDenominator node) * value + sum)
            | [] -> sum

        interpolationLoop table 0.0

    member public Interpolator.interpolateNewton table x =
        0