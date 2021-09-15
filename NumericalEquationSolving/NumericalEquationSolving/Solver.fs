namespace NumericalEquationSolving

type public Solver() =

    member public Solver.solve (f : IFunction) (A : int) (B : int) (epsilon : float32) (N : int) =
        0

    member public Solver.partition (f : IFunction) A B N =
        let h = (B - A) / N

        let rec partitionLoop x1 x2 y1 answers =
            if x2 > B then
                answers
            else
                let y2 = f.compute x2

                match y1 * y2 with
                | result when result > 0.0 -> partitionLoop x2 (x2 + h) y2 answers
                | _                        -> partitionLoop x2 (x2 + h) y2 ((x1, x2) :: answers)

        partitionLoop A (A + h) (f.compute A) []

    member public Solver.refineBisection (f : IFunction) partition epsilon =
        let rec refineBisectionLoop (a, b) starting step =
            if (b - a) < (2.0 * epsilon) then
                (starting, step, (a + b) / 2.0, (b - a), abs <| f.compute ((a + b) / 2.0))
            else
                let c = (a + b) / 2.0

                match f.compute(a) * f.compute(c) with
                | result when result > 0.0 -> refineBisectionLoop (c, b) starting (step + 1)
                | _                        -> refineBisectionLoop (a, c) starting (step + 1)

        List.map (fun (a, b) -> refineBisectionLoop (a, b) ((a + b) / 2.0) 0) partition

    member public Solver.refineNewton (f : IFunction) partition epsilon =
        let rec refineNewtonLoop previous starting step =
            let current = previous - f.compute(previous) / f.derivative(previous)

            if (abs (current - previous)) < epsilon then
                (starting, step, current, current - previous, (abs <| f.compute current))
            else
                refineNewtonLoop current starting (step + 1)

        List.map (fun (a, b) -> refineNewtonLoop ((a + b) / 2.0) ((a + b) / 2.0) 0) partition

    member public Solver.refineNewtonModified (f : IFunction) partition epsilon =
        let rec refineNewtonModifiedLoop previous starting step =
            let current = previous - f.compute(previous) / f.derivative(starting)

            if (abs (current - previous)) < epsilon then
                (starting, step, current, current - previous, (abs <| f.compute current))
            else
                refineNewtonModifiedLoop current starting (step + 1)

        List.map (fun (a, b) -> refineNewtonModifiedLoop ((a + b) / 2.0) ((a + b) / 2.0) 0) partition

    member public Solver.refineSecants (f : IFunction) partition epsilon =
        let rec refineSecantsLoop previous current starting step =
            let next = current - (f.compute(current) / (f.compute(current) - f.compute(previous))) * (current - previous)

            if (abs (current - previous)) < epsilon then
                (starting, step, next, next - current, (abs <| f.compute next))
            else
                refineSecantsLoop current next starting (step + 1)

        List.map (fun (a, b) -> refineSecantsLoop a ((a + b) / 2.0) a 0) partition
