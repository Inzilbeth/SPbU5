namespace NumericalEquationSolving

type public IFunction =
    abstract member Name : string

    abstract member compute : double -> double

    abstract member derivative : double -> double
