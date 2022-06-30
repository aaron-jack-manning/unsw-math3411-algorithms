module PseudoPrimeTest

let inline reduceMod modulus number =
    (number % modulus + modulus) % modulus

let division a b =
    let remainder = reduceMod b a
    let quotient = (a - remainder)/ b
    
    quotient, remainder

let rec gcd a b =

    let quotient, remainder = division a b

    match remainder with
    | a when a = 0 -> b
    | _ -> gcd b remainder

let pseudoPrimeTest (number : int) (a : int) =
    if gcd number a <> 1 then
        false
    elif (pown (bigint a) (number - 1) |> reduceMod (bigint number)) <> 1I then
        false
    else
        true

