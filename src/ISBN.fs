module ISBN

let reduceMod modulus number =
    (number % modulus + modulus) % modulus

let toInt character = 
    int character - int '0'
    
let rec stringToDigitList (input : string) =
    
    if input |> String.length = 0 then
        []
    elif input.[0] = ' ' || input.[0] = '-' then
        stringToDigitList input.[1..]
    elif input.[0] = 'X' || input.[0] = 'x' then
        List.append [10] (stringToDigitList input.[1..])
    else
        List.append [input.[0] |> toInt] (stringToDigitList input.[1..])
    
let isbn10Check isbn =
    
    let checkSum =
        isbn
        |> stringToDigitList
        |> List.indexed
        |> List.map (fun (index, element) -> (int index + 1) * element)
        |> List.sum
        |> reduceMod 11
    
    let numberOfDigits =
        isbn
        |> stringToDigitList
        |> List.length
    
    numberOfDigits = 10 && checkSum = 0
    
let isbn13Check isbn =

    let checkSum =
        isbn
        |> stringToDigitList
        |> List.indexed
        |> List.map (fun (index, element) -> element * if index % 2 = 1 then 3 else 1)
        |> List.sum
        |> reduceMod 10
    
    let numberOfDigits =
        isbn
        |> stringToDigitList
        |> List.length
    
    numberOfDigits = 13 && checkSum = 0