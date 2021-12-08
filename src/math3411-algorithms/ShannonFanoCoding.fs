module ShannonFanoCoding

open System

type Tree =
    | Leaf
    | Branch of (int * Tree) list

let rec listRemoveFirst element list =
    match list with
    | [] -> []
    | h :: t -> if h = element then t else h :: listRemoveFirst element t

let codewordLength (logBase : int) (probability : float) :  int =

    let rec checkLength (length : int) : int =
        if float (pown logBase length) >= (1.0 / probability) then
            length
        else
            checkLength (length + 1)

    checkLength 1

let rec emptyTree (remainingStages : int) (radix : int) =
    match remainingStages with
    | 0 -> Leaf
    | _ ->
        let mutable branches = List.empty

        for i = radix - 1 downto 0 do
            branches <- (i, emptyTree (remainingStages - 1) radix) :: branches

        Branch branches

let rec constructCode (tree : Tree) (symbols : char list) (lengths : int list) =
    
    let mutable remainingLengths = lengths
    let mutable currentBranch = List.empty
    let mutable encodingBranches = List.empty
    
    let rec codeBuilder (currentlength : int) = function
        | Leaf ->
            ()
        | Branch branches ->
            for branch in branches do
                if remainingLengths |> List.length <> 0 then
                    let digit, branchTree = branch

                    currentBranch <- currentBranch @ [digit]

                    if remainingLengths |> List.contains currentlength then
                        remainingLengths <- remainingLengths |> listRemoveFirst currentlength

                        encodingBranches <- encodingBranches @ [currentBranch]
                        currentBranch <- List.empty
                    else
                        codeBuilder (currentlength + 1) branchTree

    codeBuilder 1 tree

    let mutable encoding = List.empty

    for i = 0 to List.length encodingBranches - 1 do
        if List.length encodingBranches.[i] = lengths.[i] then
            encoding <- encoding @ [encodingBranches.[i]]
        else
            let symbolsToGrab = lengths.[i] - List.length encodingBranches.[i]

            encoding <- encoding @ [encoding.[i - 1].[..(symbolsToGrab - 1)] @ encodingBranches.[i]]

    encoding

let standardICode (symbols : char list) (lengths : int list) (radix : int) =
    
    let greatestLength = List.max lengths
    let tree = emptyTree greatestLength radix
    
    List.zip symbols (constructCode tree symbols lengths)

let shannonFanoCode (symbolsWithProbabilities : (char * float) list) (radix : int) =
    
    let symbols, lengths =
        symbolsWithProbabilities
        |> List.map (fun (symbol, probability) -> symbol, codewordLength radix probability)
        |> List.sortBy (fun (symbol, length) -> length)
        |> List.unzip

    standardICode symbols lengths radix

let shannonEntropy (probabilities : float list) (radix : int) : float =
    probabilities
    |> List.sumBy (fun prob -> - prob * (Math.Log(prob))/(Math.Log(float radix)))