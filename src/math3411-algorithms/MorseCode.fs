module MorseCode

open System
open System.Threading

let characterToMorseCode character =

    let timeUnit = 200
    let frequency = 1000

    let dit () = Console.Beep (frequency, timeUnit)
    let dah () = Console.Beep (frequency, 3 * timeUnit)
    let space () = Thread.Sleep (4 * timeUnit)

    let characterMorseCodeMap =
        [
            ('a', [dit; dah]);
            ('b', [dah; dit; dit; dit]);
            ('c', [dah; dit; dah; dit]);
            ('d', [dah; dit; dit]);
            ('e', [dit]);
            ('f', [dit; dit; dah; dit]);
            ('g', [dah; dah; dit]);
            ('h', [dit; dit; dit; dit]);
            ('i', [dit; dit]);
            ('j', [dit; dah; dah; dah]);
            ('k', [dah; dit; dah]);
            ('l', [dit; dah; dit; dit]);
            ('m', [dah; dah]);
            ('n', [dah; dit]);
            ('o', [dah; dah; dah]);
            ('p', [dit; dah; dah; dit]);
            ('q', [dah; dah; dit; dah]);
            ('r', [dit; dah; dit]);
            ('s', [dit; dit; dit]);
            ('t', [dah]);
            ('u', [dit; dit; dah]);
            ('v', [dit; dit; dit; dah]);
            ('w', [dit; dah; dah]);
            ('x', [dah; dit; dit; dah]);
            ('y', [dah; dit; dah; dah]);
            ('z', [dah; dah; dit; dit;]);
            (' ', [space]);
            ('1', [dit; dah; dah; dah; dah]);
            ('2', [dit; dit; dah; dah; dah]);
            ('3', [dit; dit; dit; dah; dah]);
            ('4', [dit; dit; dit; dit; dah]);
            ('5', [dit; dit; dit; dit; dit]);
            ('6', [dah; dit; dit; dit; dit]);
            ('7', [dah; dah; dit; dit; dit]);
            ('8', [dah; dah; dah; dit; dit]);
            ('9', [dah; dah; dah; dah; dit]);
            ('0', [dah; dah; dah; dah; dah]);
            (',', [dah; dah; dit; dit; dah; dah]);
            ('.', [dit; dah; dit; dah; dit; dah]);
            ('?', [dit; dit; dah; dah; dit; dit]);
            (';', [dah; dit; dah; dit; dah; dit]);
            (':', [dah; dah; dah; dit; dit; dit]);
            ('/', [dah; dit; dit; dah; dit]);
            ('-', [dah; dit; dit; dit; dit; dah]);
            ('\'', [dit; dah; dah; dah; dah; dit]);
            ('\"', [dit; dah; dit; dit; dah; dit]);
            ('(', [dah; dit; dah; dah; dit]);
            (')', [dah; dit; dah; dah; dit; dah]);
            ('=', [dah; dit; dit; dit; dah]);
            ('+', [dit; dah; dit; dah; dit]);
            ('@', [dit; dah; dah; dit; dah; dit])
        ] |> Map.ofList

    let sounds = Map.find (character |> Char.ToLower) characterMorseCodeMap

    for sound in sounds do
        sound ()

    Thread.Sleep (3 * timeUnit)

let rec playAsMorseCode text =
    
    let textLength = String.length text

    if textLength = 0 then
        ()
    else
        characterToMorseCode text.[0]

        playAsMorseCode text.[1..(textLength - 1)]