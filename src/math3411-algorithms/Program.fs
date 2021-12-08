open PseudoPrimeTest
open ArithmeticCoding
open HuffmanCoding
open ShannonFanoCoding
open MillerRabinTest
open FermatFactorisation
open KraftMcMillanCoefficient
open RSA
open ISBN
open MorseCode
open BaseBAlgorithm


// RSA Encryption
let result = encryption 257I 193I 35747I
match result with
| Ok (encryptor, decryptor) ->
    //Use encryptor and decryptor here
    printfn "%A" (decryptor 11170I)

    printfn "Encryptor and decryptor generated"
| Error message ->
    printfn "%s" message


// ISBN Checks
printfn "%b" (isbn10Check "0913349109")


// Sieve Of Eratosthenes
printfn "%A" (primeSieve 23)


// Fermat Factorisation
printfn "%A" (fermatFactorisation 49601)


// Kraft McMillan Coefficient
printfn "%A" (kraftMcMillanCoefficient [2; 4; 5] 3)


// Miller Rabin Test // returns true for probably prime
printfn "%A" (millerRabinTest 8 2)


// Huffman Coding
printfn "%A" (huffmanEncoding 2 ['1', 6.0m/19.0m; '2', 6.0m/19.0m; '3', 3.0m/19.0m; '4', 2.0m/19.0m; '5', 1.0m/19.0m; '6', 1.0m/19.0m ])


// Arithmetic Coding
let encoder, decoder = arithmeticCoding ['a'; 'b'; '*'] [0.1; 0.1; 0.8]
printfn "%A" (encoder "baa*")


// Shannon Fano Code
printfn "%A" (shannonFanoCode ['a', 15.0/39.0; 'b', 7.0/39.0; 'c', 6.0/39.0; 'd', 6.0/39.0; 'e', 5.0/39.0] 2)


// Shannon Entropy
printfn "%A" (shannonEntropy [3.0/10.0; 7.0/10.0] 2)


// Huffman Encoding From Markov Source
printfn "%A" (encodeFromMarkovSource ['1'; '2'; '3'] 2 (array2D [|[|5.0m/12.0m; 1.0m/4.0m; 3.0m/4.0m|]; [|1.0m/4.0m; 2.0m/3.0m; 1.0m/12.0m|]; [|1.0m/3.0m; 1.0m/12.0m; 1.0m/6.0m|]|]) [|39.0m/92.0m; 17.0m/46.0m; 19.0m/92.0m|])


// Pseudo Prime Test
printfn "%A" (pseudoPrimeTest 65 2)


// Morse Code
playAsMorseCode "This is a test of the Morse Code player."


// Base B Algorithm
printfn "%A" (baseBAlgorithm 13 2)