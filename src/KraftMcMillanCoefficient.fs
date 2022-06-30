module KraftMcMillanCoefficient


[<StructuredFormatDisplay("{Numerator}/{Denominator}")>]
type Fraction<'a> =
    { Numerator : 'a; Denominator : 'a}

    static member inline private reduceMod modulus number =
        (number % modulus + modulus) % modulus

    static member inline private division a b =
        let remainder = Fraction<_>.reduceMod b a
        let quotient = (a - remainder)/ b
            
        quotient, remainder

    static member private greatestCommonDenominator (a : 'b) (b : 'b) : 'b =
        let quotient, remainder = Fraction<_>.division a b

        if remainder = LanguagePrimitives.GenericZero then
            b
        else
            Fraction<_>.greatestCommonDenominator b remainder

    static member inline private lowestCommonMultiple a b =
        (a * b) / (Fraction<_>.greatestCommonDenominator a b)

    static member simplify (a : Fraction<_>) : Fraction<_> =
        let gcd = Fraction<_>.greatestCommonDenominator a.Numerator a.Denominator
            
        let newNumerator = a.Numerator / gcd
        let newDenominator = a.Denominator / gcd
            
        if newDenominator < LanguagePrimitives.GenericZero then
            { Numerator = -newNumerator; Denominator = -newDenominator}
        else
            { Numerator = newNumerator; Denominator = newDenominator}

    static member (/) (a : Fraction<_>, b : Fraction<_>) : Fraction<_> =
        Fraction<_>.simplify { Numerator = a.Numerator * b.Denominator; Denominator =  a.Denominator * b.Numerator }
    
    static member (*) (a : Fraction<_>, b : Fraction<_>) : Fraction<_> =
        Fraction<_>.simplify { Numerator = a.Numerator * b.Numerator; Denominator = a.Denominator * b.Denominator }
    
    static member (+) (a : Fraction<_>, b : Fraction<_>) : Fraction<_> =
        Fraction<_>.simplify { Numerator = a.Numerator * b.Denominator + a.Denominator * b.Numerator; Denominator = a.Denominator * b.Denominator }
    
    static member (-) (a : Fraction<_>, b : Fraction<_>) : Fraction<_> =
        Fraction<_>.simplify { Numerator = a.Numerator * b.Denominator - a.Denominator * b.Numerator; Denominator = a.Denominator * b.Denominator }
        
    static member fromInt (integer : int) = { Numerator = integer; Denominator = LanguagePrimitives.GenericOne }
    static member fromInt64 (integer : int64) = { Numerator = integer; Denominator = LanguagePrimitives.GenericOne }
    static member fromBigint (integer : bigint) = { Numerator = integer; Denominator = LanguagePrimitives.GenericOne }
    static member fromIntTuple (numerator : int, denominator : int) = { Numerator = numerator; Denominator = denominator }
    static member fromInt64Tuple (numerator : int64, denominator : int64) = { Numerator = numerator; Denominator = denominator }
    static member fromBigintTuple (numerator : bigint, denominator : bigint) = { Numerator = numerator; Denominator = denominator }

    static member one = Fraction<_>.fromIntTuple (1, 1)
    static member zero = Fraction<_>.fromIntTuple (0, 1)
    
    static member ( ^^ ) (a : Fraction<_>, power : int) : Fraction<_> =
        if power < LanguagePrimitives.GenericZero then
            let positivePower = -power
            { Numerator = pown a.Denominator positivePower; Denominator = pown a.Numerator positivePower }
        else
            { Numerator = pown a.Numerator power; Denominator = pown a.Denominator power }
    
    static member asFloat (a : Fraction<_>) : float =
        (float a.Numerator) / (float a.Denominator)
    
    static member asDecimal (a : Fraction<_>) : decimal =
        (decimal a.Numerator) / (decimal a.Denominator)
          
    static member isWhole (a : Fraction<_>) : bool =
        a.Denominator - LanguagePrimitives.GenericOne = LanguagePrimitives.GenericZero

let rec kraftMcMillanCoefficient lengths radix =
    
    match lengths with
    | [] -> Fraction<int>.zero
    | head :: tail -> 
        ((Fraction<int>.fromIntTuple (1, radix)) ^^ head) + (kraftMcMillanCoefficient tail radix)