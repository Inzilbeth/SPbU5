namespace NumericalEquationSolving

type public FirstFunction() =

    interface IFunction with
        member this.Name =
            "64*x^7 - 112*x^5 + 56*x^3 - 7*x + sqrt(1-0.2x^2)"

        member this.compute x =
            64.0 * x ** 7.0 - 112.0 * x ** 5.0 + 56.0 * x ** 3.0 - 7.0 * x + sqrt(1.0 - 0.2 * x ** 2.0)

        member this.derivative x =
            - (x / (5.0 * sqrt(1.0 - 0.2 * (x ** 2.0 / 5.0)))) + 448.0 * x ** 6.0 - 560.0 * x ** 4.0 + 168.0 * x ** 2.0 - 7.0
