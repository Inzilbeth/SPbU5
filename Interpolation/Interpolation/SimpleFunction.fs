namespace Interpolation

type public FirstFunction() =

    interface IFunction with
        member this.Name =
            "sqrt(1 + x^2)"

        member this.compute x =
            1.0 + x ** 2.0 |> sqrt
