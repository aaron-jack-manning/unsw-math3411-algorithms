module ArithmeticCoding

let indexOfSymbol symbol symbols =
    symbols |> List.findIndex (fun x -> x = symbol)

let divideInterval lower upper probabilities =
    
    let range = upper - lower

    let rec intervalDivision (lower : float) (upper : float) (range : float) (remainingProbabilities : float list) : float list =
    
        match remainingProbabilities with
        | [] -> []
        | head :: tail ->
            let newValue = lower + head * range
            List.append [newValue] (intervalDivision (newValue) upper range tail)

    List.append [lower] (intervalDivision lower upper range probabilities)

let encode (symbols : char list) (probabilities : float list) (message : string) : float =
    
    let messageList = Seq.toList message

    let mutable lower = 0.0
    let mutable upper = 1.0
    
    for currentSymbol in messageList do
        let index = indexOfSymbol currentSymbol symbols

        let dividedInterval = divideInterval lower upper probabilities

        lower <- dividedInterval.[index]
        upper <- dividedInterval.[index + 1]

    (lower + upper) / 2.0

let decode (symbols : char list) (probabilities : float list) (encoded : float) : char list =
    
    let mutable lower = 0.0
    let mutable upper = 1.0

    let mutable oldUpper = 0.0

    let mutable decoded = []

    while oldUpper <> upper do
        let dividedInterval = divideInterval lower upper probabilities

        oldUpper <- upper

        upper <- dividedInterval |> List.find (fun x -> x > encoded)
        let indexOfLower = (dividedInterval |> List.findIndex (fun x -> x = upper)) - 1
        lower <- dividedInterval.[indexOfLower]

        decoded <- [symbols.[indexOfLower]] |> List.append decoded

    decoded

// This is the primary entry point for the functions in this module.
let arithmeticCoding symbols probabilities =
    
    let decoder = decode symbols probabilities
    let encoder = encode symbols probabilities

    (encoder, decoder)