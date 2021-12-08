module HuffmanCoding

type Tree = 
    | Branch of decimal * string * Tree list
    | Leaf of decimal * string * char

type HuffmanCode = { AlgorithmTree : Tree; Encoding : (char * string) list; AverageCodewordLength : decimal; Encoder : string -> string; Decoder : string -> string }

let treeToProbability tree =
    match tree with
    | Leaf (probability, encoding, symbol) ->
        probability
    | Branch (probability, encoding, trees) ->
        probability

let sortTrees (trees : Tree list) : Tree list =
    trees
    |> List.sortWith
        (fun a b -> if (treeToProbability a) > (treeToProbability b) then 1 else -1) // this is so the sort is stable

let insertTreeIntoList (existingTrees : Tree list) (newTree : Tree) : Tree list =
    match existingTrees with
    | [] ->
        [newTree]
    | _ ->
        let valueToInsert =
            treeToProbability newTree

        let insertionIndex =
            try
                existingTrees
                |> List.findIndexBack
                    (
                        fun tree ->
                            (treeToProbability tree) <= valueToInsert
                    )
            with 
            | _ -> -1
        
        if insertionIndex = -1 then
            List.append [newTree] existingTrees
        else
            let firstPartOfList = List.append existingTrees.[..insertionIndex] [newTree]
        
            List.append firstPartOfList (if List.length existingTrees - 1 < insertionIndex then [] else existingTrees.[(insertionIndex + 1)..])

let combineTrees (layer : Tree list) (radix : int) : Tree list =
    
    let toCombine = layer.[..(radix - 1)]
    let remaining = if List.length layer < radix then [] else layer.[radix..]

    let combinedProbability =
        toCombine
        |> List.sumBy treeToProbability

    let combinedNode =
        Branch (combinedProbability, "", toCombine)

    insertTreeIntoList remaining combinedNode

let convertIntitialProbabilitiesToTrees (symbolsAndProbabilities : (char * decimal) list) =
    symbolsAndProbabilities
    |> List.map
        (
            fun x ->
            let symbol, probability = x

            Leaf (probability, "", symbol)
        )

let calculateDummySymbols numberOfSymbols radix =
    if radix = 2 then
        0
    else
        let mutable numberOfDummySymbols = 0
        
        while (numberOfSymbols + numberOfDummySymbols) % (radix - 1) <> 1 do
            numberOfDummySymbols <- numberOfDummySymbols + 1

        numberOfDummySymbols


let createTree (symbolsAndProbabilities : (char * decimal) list) (radix : int) : Tree =
    
    let initialProbabilitiesTree = convertIntitialProbabilitiesToTrees symbolsAndProbabilities

    let numberOfDummySymbols = calculateDummySymbols (symbolsAndProbabilities |> List.length) (radix)

    let sortedProbabilities =
        sortTrees initialProbabilitiesTree
        |> List.append
            [
                for i = 1 to numberOfDummySymbols do
                    Leaf (0.0m, "", '_')
            ]

    let rec treeGenerator (layer : Tree list) (radix : int) : Tree =
        
        if List.length layer = 1 then
            layer.[0]
        else
            treeGenerator (combineTrees layer radix) radix

    treeGenerator sortedProbabilities radix


let generateEncoding (tree : Tree) =    
    let mutable fullEncoding : ((char * string) list) = List.empty

    let rec traverse (tree : Tree) =
        match tree with
        | Leaf (probability, encoding, symbol) ->
            fullEncoding <- List.append fullEncoding [(symbol, encoding)]
            ()
        | Branch (probability, encoding, subTrees) ->
            for i = 0 to List.length subTrees - 1 do
                let newEncoding = encoding + string (List.length subTrees - 1 - i)


                let subTree =
                    match subTrees.[i] with
                    | Leaf (probability, encoding, symbol) ->
                        Leaf (probability, newEncoding, symbol)
                    | Branch (probability, encoding, trees) ->
                        Branch (probability, newEncoding, trees)
            
                traverse (subTree)

    traverse tree

    fullEncoding
    |> List.where (fun (symbol, encoding) -> symbol <> '_')
    |> List.sortBy (fun (symbol, encoding) -> symbol)

let rec averageLength = function
    | Leaf (probability, encoding, symbol) ->
        0.0m
    | Branch (probability, encoding, subTrees) ->
        
        ([
            for subTree in subTrees do
                yield averageLength subTree
        ]
        |> List.sum)
        + probability

let rec encoder (encoding : (char * string) list) (message : string) : string =
    match message |> Seq.toList with
    | head :: tail ->
        let currentSymbol, codeword =
            encoding
            |> List.find (fun (symbol, encoding) -> symbol = head)
        let remainingMessage =
            ("", tail)
            ||> List.fold (fun state element -> state + string element)

        codeword + (encoder encoding remainingMessage)
    | _ -> ""

let rec decoder (encoding : (char * string) list) (encodedMessage : string) : string =

    let shorten (toShorten : string) (length : int) =
        if String.length toShorten <= length then
            ""
        else
            toShorten.[length..]

    let tryMatch (testString : string) (matchCase : string) =
        let shorterStringLength = if String.length matchCase < String.length testString then String.length matchCase else String.length testString

        [for i = 0 to shorterStringLength - 1 do testString.[i] = matchCase.[i]]
        |> List.forall id

    if String.length encodedMessage = 0 then
        ""
    else
        let mutable decoding = ""
        for encodingPair in encoding do
            let symbol, codeword = encodingPair

            if tryMatch encodedMessage codeword then
                decoding <- string symbol + decoder encoding (shorten encodedMessage (String.length codeword))
        
        decoding

// This is the function to call to create the Huffman encoding for a given set of symbols and probabilities, with a specified radix.
let huffmanEncoding (radix : int) (symbolsAndProbabilities : (char * decimal) list) =
    let tree = createTree symbolsAndProbabilities radix

    let encoding = generateEncoding tree
    let averageCodewordLength = averageLength tree
    
    let encoderFunction = encoder encoding
    let decoderFunction = decoder encoding

    { AlgorithmTree = tree; Encoding = encoding; AverageCodewordLength = averageCodewordLength; Encoder = encoderFunction; Decoder = decoderFunction }

let checkMarkovSource (markovMatrix : decimal [,]) =
    [
        for i = 0 to Array2D.length2 markovMatrix - 1 do
            markovMatrix.[*, i] |> Array.sum = 0.0m
    ] |> List.forall id

let encodeFromMarkovSource (symbols : char list) (radix : int) (markovMatrix : decimal [,]) (equilibriumState : decimal []) =
    [
        yield
            (symbols, equilibriumState |> Array.toList)
            ||> List.zip
            |> huffmanEncoding radix
         
        for i = 0 to Array2D.length2 markovMatrix - 1 do
            yield
                (symbols, markovMatrix.[*, i] |> Array.toList)
                ||> List.zip
                |> huffmanEncoding radix
    ]