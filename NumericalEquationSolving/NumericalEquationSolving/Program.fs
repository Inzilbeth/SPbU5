namespace NumericalEquationSolving

module Program =
    [<EntryPoint>]
    let main argv =
        printfn "Topic: \"Numerical methods for solving nonlinear equations\"\n"

        let solver = new Solver()
        let f  = new FirstFunction()

        let A = -1.2
        let B = 1.0
        let N = 1e3
        let e = 1e-06

        let printPartition partition =
            let rec printPartitionLoop partition number =
                match partition with
                | (a, b) :: tail -> printfn "Segment #%A: a = %A, b = %A" number a b
                                    printPartitionLoop tail (number + 1)
                | [] -> ()

            printPartitionLoop partition 1


        let printAnswer answer =
            let rec printAnswerLoop answer number =
                match answer with
                | (starting, steps, answer, delta, absolute) :: tail -> printfn "\nRoot #%A:" number
                                                                        printfn "x_0 = %A" starting
                                                                        printfn "k = %A" steps
                                                                        printfn "x_k = %A" answer
                                                                        printfn "x_k - x_k-1 = %A" delta
                                                                        printfn "|f(x_k)| = %A" absolute

                                                                        printAnswerLoop tail (number + 1)
                | [] -> ()

            printAnswerLoop answer 1

        let partition = solver.partition f A B N

        let bisectionAnswers      = solver.refineBisection f partition e
        let newtonAnswers         = solver.refineNewton f partition e
        let newtonModifiedAnswers = solver.refineNewtonModified f partition e
        let secantsAnswers        = solver.refineSecants f partition e

        printfn "A = %A, B = %A, f(x) = %s, e = %A\n" A B (f :> IFunction).Name e

        printfn "Calculated partition:\n"
        printPartition partition
        printfn ""

        printfn "Results using bisections method:"
        printAnswer bisectionAnswers

        printfn "\nResults using Newton's method:"
        printAnswer newtonAnswers

        printfn "\nResults using modified Newton's method:"
        printAnswer newtonModifiedAnswers

        printfn "\nResults using secants method:"
        printAnswer secantsAnswers

        0