module BaseBAlgorithm

let reduceMod modulus number =
    (number % modulus + modulus) % modulus

let division a b =
    let remainder = reduceMod b a
    let quotient = (a - remainder)/ b
    
    quotient, remainder

let baseBAlgorithm (number : int) (radix : int) =
    let digits = List.empty

    let rec convert (current : int) =

        let quotient, remainder = division current radix

        match quotient with
        | 0 -> digits @ [remainder]
        | _ -> (convert quotient) @ [remainder]

    convert number