
type TraceBuilderWithFirstNonFailWins() =
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
        None  // failure

    member this.Yield(x) =
        printfn "Yield an unwrapped %A as an option" x
        Some x

    member this.YieldFrom(m) =
        printfn "Yield an option (%A) directly" m
        m

    member this.Combine (a,b) =
        printfn "Combining %A with %A" a b
        match a with
        | Some _ -> a  // a succeeds -- use it
        | None -> b    // a fails -- use b instead

    member this.Delay(f) =
        printfn "Delay"
        f()

let trace = new TraceBuilderWithFirstNonFailWins()

type IntOrBool = 
    | I of int
    | B of bool

let parseInt (s : string) =
    match System.Int32.TryParse(s) with
    | true,i -> Some (I i)
    | false,_ -> None

let parseBool (s : string) =
    match System.Boolean.TryParse(s) with
    | true,i -> Some (B i)
    | false,_ -> None

trace {
    return! parseBool "42"  // fails
    return! parseInt "42"
    } |> printfn "Result for parsing: %A"

// As with all the builder methods, if you don’t need them,
// you don’t need to implement them. So for a workflow 
// that is strongly sequential, you could easily create 
// a builder class with Combine, Zero,
// and Yield, say, without having to implement 
// Bind and Return at all.

type MinimalTraceBuilder() =

    member this.ReturnFrom(x) = x

    member this.Zero() = Some ()

    member this.Combine (a,b) =
        a |> Option.bind (fun ()-> b )

    member this.Delay(f) = f()

// make an instance of the workflow
let tracemin = new MinimalTraceBuilder()

tracemin {
    if true then printfn "hello......."
    if false then printfn ".......world"
    return! Some 1
    } |> printfn "Result for minimal combine: %A"

type MinimalListBuilder() =

    member this.Yield(x) = [x]

    member this.For(m,f) =
        m |> List.collect f

    member this.Combine (a,b) =
        List.concat [a;b]

    member this.Delay(f) = f()

// make an instance of the workflow
let listbuilder = new MinimalListBuilder()

listbuilder {
    yield 1
    yield 2
    } |> printfn "Result: %A"

listbuilder {
    for i in [1..5] do yield i + 2
    yield 42
    } |> printfn "Result: %A"

module StandaloneCombine =

    let combine a b =
        match a with
        | Some _ -> a  // a succeeds -- use it
        | None -> b    // a fails -- use b instead

    // create an infix version
    let ( <++ ) = combine

    let map1 = [ ("1","One"); ("2","Two") ] |> Map.ofList
    let map2 = [ ("A","Alice"); ("B","Bob") ] |> Map.ofList

    let result =
        (map1.TryFind "A")
        <++ (map1.TryFind "B")
        <++ (map2.TryFind "A")
        <++ (map2.TryFind "B")
        |> printfn "Result of adding options is: %A"