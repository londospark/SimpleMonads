module SimpleMonads

type Expr =
    | Val of int
    | Div of Expr * Expr

// Gives us "None" instead of "<null>" when we do a print
type Option<'a> = 
    | Some of 'a
    | None
let safeDiv a b =
    match b with
    | 0 -> None
    | _ -> Some (a / b)

let bind (func: 'T -> Option<'U>) (opt: Option<'T>) =
    match opt with
    | None -> None
    | Some value -> func(value)

let return' x = Some x

let (>>=) opt func = bind func opt

type OptionBuilder() =
    member x.Bind(comp, func) = bind func comp
    member x.Return(value) = return' value
    member x.ReturnFrom(value) = value

let option = OptionBuilder()

let rec eval expr =
    match expr with
    | Val x -> return' x
    | Div (a, b) ->
        option {
            let! numerator = eval a
            let! denominator = eval b
            return! safeDiv numerator denominator
        }

        // eval a >>=
        //     (fun numerator ->
        //         eval b >>=
        //             (fun denominator ->
        //                 safeDiv numerator denominator))

        // match eval a with
        // | None -> None
        // | Some numerator ->
        //     match eval b with
        //     | None -> None
        //     | Some denominator -> safeDiv numerator denominator

[<EntryPoint>]
let main _args =
    printfn "eval 5 is %A" (eval (Val 5))
    printfn "eval 5/1 is %A" (eval (Div (Val 5, Val 1)))
    printfn "eval 6/(9/3) is %A" (eval (Div(Val 6, Div (Val 9, Val 3))))
    printfn "eval 6/(9/0) is %A" (eval (Div(Val 6, Div (Val 9, Val 0))))
    0 // return an integer exit code
