namespace Interpolation

open System

module Program =
    [<EntryPoint>]
    let main argv =
        let printTable table =
            let rec printTableLoop table count =
                match table with
                | (node, value) :: tail -> printfn "#%A Node: %A, Value: %A" count node value
                                           printTableLoop tail (count + 1)
                | [] -> printfn "\n"

            printTableLoop table 1

        let interpolator = new Interpolator()
        let f = new SimpleFunction()
        let n = 8

        printfn "Algebraic interpolation problem"
        printfn "Variant 4, f(x) = %s\n" (f :> IFunction).Name

        printfn "Enter the number of nodes in the table (m + 1): "
        let m = Double.Parse(Console.ReadLine())

        printfn "Enter the start of the interval (a): "
        let a = Double.Parse(Console.ReadLine())

        printfn "Enter the end of the interval (b): "
        let b = Double.Parse(Console.ReadLine())

        let table = interpolator.getValues a b m f

        printfn "\nOriginal table of nodes and exact values of a function: "
        printTable table

        let rec loopValues() =
            printfn "Enter the node where the approximation of a function is required (x): "
            let x = Double.Parse(Console.ReadLine())

            let rec askForN() =
                let n = Int32.Parse(Console.ReadLine())

                if float n > m then
                    printfn "Invalid input, input value should be <= %A. Input a different value: " m
                    askForN()
                else n

            printfn "Enter the maximum degree of a polynomial, [should be <= %A] (n): " m
            let n = askForN()

            let preparedTable = interpolator.prepareTable table x n

            printfn "\nSorted and reduced table:"
            printTable preparedTable

            let interpolatedValueLagrange = interpolator.interpolateLagrange preparedTable x
            let interpolatedValueNewton   = interpolator.interpolateNewton   preparedTable x

            printfn "Approximate value of f(x) using the Lagrange representation: %.20f" interpolatedValueLagrange
            printfn "Absolute error: %e" (abs (((f :> IFunction).compute x) - interpolatedValueLagrange))
            printfn ""
            printfn "Approximate value of f(x) using the Newton representation:   %.20f" interpolatedValueNewton
            printfn "Absolute error: %e" (abs (((f :> IFunction).compute x) - interpolatedValueNewton))

            printfn "\nDo you want to enter new values for x, n? (yes/no): "
            match Console.ReadLine() with
            | s when s = "yes" -> loopValues()
            | s when s = "no" -> ()
            | _ -> printfn "Invalid answer. Exiting the program ..."

        loopValues()
        0
