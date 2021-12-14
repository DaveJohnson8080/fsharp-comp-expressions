type LoggingBuilder () =
    let log p = printfn "expression is %A" p

    member __.Bind(x, f) =
        log x
        f x

    member __.Return(x) =
        x

let logger = new LoggingBuilder()

let loggedWorkflow = logger {
    let! x = 42
    let! y = 43
    let! z = x + y
    return z
}

module DivideByWithBindFunction =

    let divideBy bottom top =
        if bottom = 0
        then None
        else Some(top/bottom)

    let bind (m,f) =
        Option.bind f m

    let return' x = Some x

    let divideByWorkflow x y w z =
        bind (x |> divideBy y, fun a ->
        bind (a |> divideBy w, fun b ->
        bind (b |> divideBy z, fun c ->
        return' c
        )))

    // test
    let good = divideByWorkflow 12 3 2 1
    let bad = divideByWorkflow 12 3 0 1

module DivideByWithCompExpr =

    let divideBy bottom top =
        if bottom = 0
        then None
        else Some(top/bottom)

    type MaybeBuilder() =
        member this.Bind(m, f) = Option.bind f m
        member this.Return(x) = Some x

    let maybe = new MaybeBuilder()

    let divideByWorkflow x y w z =
        maybe
            {
            let! a = x |> divideBy y
            let! b = a |> divideBy w
            let! c = b |> divideBy z
            return c
            }

    // test
    let good = divideByWorkflow 12 3 2 1
    let bad = divideByWorkflow 12 3 0 1

module DivideByWithBindOperator =

    let divideBy bottom top =
        if bottom = 0
        then None
        else Some(top/bottom)

    let (>>=) m f = Option.bind f m

    let divideByWorkflow x y w z =
        x |> divideBy y
        >>= divideBy w
        >>= divideBy z

    // test
    let good = divideByWorkflow 12 3 2 1
    let bad = divideByWorkflow 12 3 0 1

module StringToInt =
    let strToInt (str : string) =
        let x, y = System.Int32.TryParse(str)
        if x then 
            Some y 
        else 
            None

    type StringToIntBuilder () =
        member __.Bind(x, f) =
            Option.bind f x
        member __.Return(x) =
            Some x

    let stringToInt = StringToIntBuilder()

    let stringAddWorkflow x y z =
        stringToInt
            {
                let! a = strToInt x
                let! b = strToInt y
                let! c = strToInt z
                return a + b + c
            }

    // test
    let good = stringAddWorkflow "12" "3" "2"
    let bad = stringAddWorkflow "12" "xyz" "2"

    let strAdd str i =
        stringToInt {
            let! a = strToInt str
            return a + i
        }

    // let (>>=) m f = ???

type ListWorkflowBuilder() =

    member this.Bind(list, f) =
        list |> List.collect f

    member this.Return(x) =
        [x]
    
    member this.For(list, f) =
        this.Bind(list, f)

let listWorkflow = new ListWorkflowBuilder()

let added =
    listWorkflow {
        let! i = [1;2;3]
        let! j = [10;11;12]
        return i+j
        }
printfn "added=%A" added

let multiplied =
    listWorkflow {
        let! i = [1;2;3]
        let! j = [10;11;12]
        return i*j
        }
printfn "multiplied=%A" multiplied

let multiplied2 =
    listWorkflow {
        for i in [1;2;3] do
        for j in [10;11;12] do
        return i*j
        }
printfn "multiplied=%A" multiplied2

