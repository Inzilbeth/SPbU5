namespace Interpolation

type public IFunction =
    abstract member Name : string

    abstract member compute : double -> double