// Learn more about F# at http://fsharp.org

open System

type Regime = New | Old

type Slab = float * float

let newTaxSlabs : Slab list = [ 
                (1500000.0, 0.30) ; 
                (1250000.0, 0.25) ; 
                (1000000.0, 0.20) ; 
                (750000.0, 0.15) ; 
                (500000.0, 0.10) ; 
              ]

let oldTaxSlabs : Slab list = [ 
                (1000000.0, 0.30) ; 
                (50000.0, 0.20) ; 
                (250000.0, 0.5) ; 
              ]

let getSlab (regime: Regime) : Slab list =
    match regime with
    | New -> newTaxSlabs
    | Old -> oldTaxSlabs

let rec calculateTax (regime: Regime) (exemptions: float list) (income: float) : float =
    let slabList = getSlab regime
    let taxableIncome = match regime with
                        | New -> income
                        | Old -> income - List.sum exemptions
    let rec calculate residue =
        let slab = List.tryFind ( fun (slabBase, rate) -> residue > slabBase ) slabList
        match (slab, residue) with
        | (Some(slabBegin, rate), x) when x > slabBegin -> ( x - slabBegin ) * rate + calculate slabBegin
        | (_, _) -> 0.0
    calculate taxableIncome

let calculateNewTax = calculateTax New []
let calculateOldTax = calculateTax Old [ 150000.0; 250000.0; 15000.0; 20000.0 ]

[<EntryPoint>]
let main argv =
    let salaries = [ for i in 1 .. 100 -> float i * 25000.0 ]
    salaries 
        |> List.map ( fun x -> sprintf "%.2f, %.2f, %.2f" x (calculateNewTax x) (calculateOldTax x) ) 
        |> List.iter ( fun x -> printfn "%s" x )
    0 // return an integer exit code
