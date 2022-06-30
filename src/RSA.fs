module RSA

let reduceMod modulus number =
    (number % modulus + modulus) % modulus

let division a b =
    let remainder = reduceMod b a
    let quotient = (a - remainder)/ b
    
    quotient, remainder

let isPrime n =
    let rec check k  =
        k > n/2I || (n % k <> 0I && check (k + 1I))

    check 2I

let rec gcd a b =

    let quotient, remainder = division a b

    match remainder with
    | a when a = 0I -> b
    | _ -> gcd b remainder

let power a b =
    let rec computePower a b result =
        if b = 0I then
            result
        else
            computePower a (b - 1I) (a * result)

    computePower a b 1I

let modularInverse modulus value = 
    let rec extendedEuclideanAlgorithm t t' r r' =
        if r' = 0I then
            t
        else
            let d = r/r'
            extendedEuclideanAlgorithm t' (t - d * t') r' (r - d * r')

    (modulus + extendedEuclideanAlgorithm 0I 1I modulus value) % modulus

let computePublicKey phi privateKey =
    modularInverse phi privateKey

let encryptOrDecrypt key modulus value =
    (power value key)
    |> reduceMod modulus

let computePhiOfModulus prime1 prime2 =
    (prime1 - 1I) * (prime2 - 1I)

// This is the primary entry point for the functions in this module.
let encryption prime1 prime2 encryptionKey =
    
    if not (isPrime prime1) || not (isPrime prime2) then
        Error "One of the numbers supplied as a prime wasn't prime."
    else
        let modulus = prime1 * prime2
        let phi = computePhiOfModulus prime1 prime2
        
        if (gcd encryptionKey phi) <> 1I then
            Error $"The encryptor must be coprime to {phi}."
        else
            let encryptor = encryptOrDecrypt encryptionKey modulus

            let decryptionKey = (modularInverse phi encryptionKey)

            let decryptor = encryptOrDecrypt decryptionKey modulus

            Ok (encryptor, decryptor)