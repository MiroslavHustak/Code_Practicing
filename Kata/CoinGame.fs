module CoinGame

open System
open Helpers

let private playGame person1 person2 = 

    let personList = match (new Random()).NextDouble() < 0.5 with true -> [ person1; person2 ] | false -> [ person2; person1 ]   
    let coinSideList = match (new Random()).NextDouble() < 0.5 with true -> [ "heads"; "tails" ] | false -> [ "tails"; "heads" ] 
    
    printfn "%s %s %s" (personList |> List.item 0) "won with a flip of" (coinSideList |> List.item 0)
    printfn "%s %s %s" (personList |> List.item 1) "lost with a flip of" (coinSideList |> List.item 1)
              
let main () =   
    
    Seq.initInfinite (fun _ -> true) |> Seq.takeWhile ((=) true)
    |> Seq.iteri 
        (fun i _ -> 
                  match i with                  
                  | 0 -> 
                       playGame "Mark" "Tom" 
                  | _ -> 
                       printfn "Play again?"                  
                       match Console.ReadLine().ToUpper() |> Option.ofNull with 
                       | Some "Y"
                           when i < Int32.MaxValue -> playGame "Mark" "Tom"                              
                       | _                         -> System.Environment.Exit(0)                   
        )