namespace NumericalEquationSolving

type public FirstFunction() =

    interface IFunction with
        member this.Name =
            "x - 10 * sin(x)"

        member this.compute x =
            x - 10.0 * sin(x)

        member this.derivative x =
            1.0 - 10.0 * cos(x)
