module MillerRabinTest

let primeSieve max =
    let primes = [|0..max|]
    primes.[1] <- 0
    let mutable index = 2

    while index * index <= max do
        
        if primes.[index] <> 0 then
            let mutable toRemove = primes.[index] * primes.[index]

            while toRemove < max do
                primes.[toRemove] <- 0
                toRemove <- toRemove + primes.[index]
        index <- index + 1

    primes
    |> Array.where (fun x -> x <> 0)

let rec tryDivide (number : int) (potentialFactors : int[]) : int =
    match potentialFactors with
    | [||] ->
        1
    | [|a|]
        ->
            if number % a = 0 then
                a
            else
                1
    | _ ->
        if number % (potentialFactors[0]) = 0 then
            potentialFactors[0]
        else
            tryDivide number potentialFactors[1..]

let inline reduceMod modulus number =
    (number % modulus + modulus) % modulus

let factorise number =
    let primes = primeSieve number

    let mutable toReduce = number
    let mutable factor = 0
    let mutable factors = List.Empty

    factor <- tryDivide toReduce primes

    while factor <> 1 do
        factors <- factor :: factors
        toReduce <- toReduce / factor

        factor <- tryDivide toReduce primes

    factors

let millerRabinTest (number : int) (a : int) : bool =
    
    let factors = factorise (number - 1)

    let s =
        factors
        |> List.where (fun x -> x = 2)
        |> List.length

    let t =
        factors
        |> List.where (fun x -> x <> 2)
        |> List.fold (fun s e -> s * e) 1


    if pown (bigint a) t |> reduceMod (bigint number) = 1I then
        true
    else
        not
            ([|
                for r = 0 to s - 1 do
                    if pown (bigint a) (pown 2 r * t) |> reduceMod (bigint number) = (bigint number - 1I) then
                        true
                    else    
                        false
            |]
            |> Array.forall (fun x -> x = false))
