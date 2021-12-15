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

type TraceBuilder() =
    member this.Bind(m, f) =
        match m with
        | None ->
            printfn "Binding with None. Exiting."
        | Some a ->
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m

    member this.Return(x) =
        printfn "Returning a unwrapped %A as an option" x
        Some x

    member this.ReturnFrom(m) =
        printfn "Returning an option (%A) directly" m
        m

    member this.Zero() =
        printfn "Zero"
        None

    member this.Yield(x) =
        printfn "Yield an unwrapped %A as an option" x
        Some x

    member this.YieldFrom(m) =
        printfn "Yield an option (%A) directly" m
        m

    member this.Combine (a,b) =
        match a,b with
        | Some a', Some b' ->
            printfn "combining %A and %A" a' b'
            Some (a' + b')
        | Some a', None ->
            printfn "combining %A with None" a'
            Some a'
        | None, Some b' ->
            printfn "combining None with %A" b'
            Some b'
        | None, None ->
            printfn "combining None with None"
            None

    member this.Delay(f) =
        printfn "Delay"
        f()

// make an instance of the workflow
let trace = new TraceBuilder()

trace {
    return 1
    } |> printfn "Result 1: %A"

trace {
    return! Some 2
    } |> printfn "Result 2: %A"

trace {
    let! x = Some 1
    let! y = Some 2
    return x + y
    } |> printfn "Result 3: %A"

trace {
    let! x = None
    let! y = Some 1
    return x + y
    } |> printfn "Result 4: %A"

trace {
    do! Some (printfn "...expression that returns unit")
    do! Some (printfn "...another expression that returns unit")
    let! x = Some (1)
    return x
    } |> printfn "Result from do: %A"

trace {
} 
|> printfn "Result for empty: %A"

trace {
    printfn "hi"
} |> printfn "Results for simple exp: %A"

trace {
    if false then return 1
    } |> printfn "Result for if without else: %A"

let s = seq {printfn "zero" }    // Error
let a = async {printfn "zero" }  // OK

trace {
    yield 1
    } |> printfn "Result for yield: %A"

trace {
    yield! Some 1
    } |> printfn "Result for yield!: %A"

trace {
    yield 1
    yield 2
    } |> printfn "Result for yield then yield: %A"

trace {
    yield 1
    let! x = None
    yield 2
    } |> printfn "Result for yield then None: %A"

trace {
    yield 1
    yield 2
    yield 3
    } |> printfn "Result for yield x 3: %A"

// We can even try mixing up yield and return together. Other than the syntax difference, the overall effect is the same.

trace {
    yield 1
    return 2
    } |> printfn "Result for yield then return: %A"

trace {
    return 1
    return 2
    } |> printfn "Result for return then return: %A"

type ListBuilder() =
    member this.Bind(m, f) =
        m |> List.collect f

    member this.Zero() =
        printfn "Zero"
        []

    member this.Yield(x) =
        printfn "Yield an unwrapped %A as a list" x
        [x]

    member this.YieldFrom(m) =
        printfn "Yield a list (%A) directly" m
        m

    member this.For(m,f) =
        printfn "For %A" m
        this.Bind(m,f)

    member this.Combine (a,b) =
        printfn "combining %A and %A" a b
        List.concat [a;b]

    member this.Delay(f) =
        printfn "Delay"
        f()

// make an instance of the workflow
let listbuilder = new ListBuilder()

listbuilder {
    yield 1
    yield 2
    } |> printfn "Result for yield then yield: %A"

listbuilder {
    yield 1
    yield! [2;3]
    } |> printfn "Result for yield then yield! : %A"

listbuilder {
    for i in ["red";"blue"] do
        yield i
        for j in ["hat";"tie"] do
            yield! [i + " " + j;"-"]
    } |> printfn "Result for for..in..do : %A"


// A subtle but important point is that they are combined “backwards”, starting from the last value. 
// First “3” is combined with “4”, and the result of that is then combined with “2”, and so on.
listbuilder {
    yield 1
    yield 2
    yield 3
    yield 4
    } |> printfn "Result for yield x 4: %A"

