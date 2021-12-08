module FermatFactorisation

open System

let ceilingOfSquareRoot (number : int) =
    int (Math.Ceiling(Math.Sqrt(float number)))

let isSquare number =
    let potentialRoot = ceilingOfSquareRoot number

    number = potentialRoot * potentialRoot

let fermatFactorisation number =
    
    let rec findFactor current differenceTerm potentialFactorSquared =
        printfn $"t = {current}, 2t + 1 = {differenceTerm}, s^2 = t^2 - n = {potentialFactorSquared}"

        if current >= number then
            Error (number, 1)
        elif isSquare potentialFactorSquared then
            Ok (ceilingOfSquareRoot potentialFactorSquared + current, Math.Abs(ceilingOfSquareRoot potentialFactorSquared - current))
        else 
            findFactor (current + 1) (differenceTerm + 2) (differenceTerm + potentialFactorSquared)
    let start = ceilingOfSquareRoot number

    findFactor start (2 * start + 1) (start * start - number)