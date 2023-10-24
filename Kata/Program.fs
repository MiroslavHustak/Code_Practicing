
open System
open System.Text.RegularExpressions

open Helpers.CopyingOrMovingFiles


printfn "copyFiles %i" <| copyFiles ()

//**********Katas from codewar and problems/algorythms from leetcode



//**************************************** Kata1 ************************************************
(*
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Finish the solution so that it returns the sum of all the multiples of 3 or 5 below the number passed in. Additionally, if the number is negative, return 0 (for languages that do have them).

Note: If the number is a multiple of both 3 and 5, only count it once.

*)

let sumOfMultiplies n = 
    [ 0 .. n - 1 ]
    |> List.filter (fun n -> n % 3 = 0 || n % 5 = 0) |> List.sum
                
printfn "%i" <| sumOfMultiplies 20

//******************* Fibonacci ****************************************************************
   
    (*   
    Create function fib that returns n'th element of Fibonacci sequence (classic programming task).
    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...    
       
    cycle = 5
    cycle = 4
    cycle = 3
    cycle = 2
    cycle = 1
    cycle = 0
    acc1 = 1  acc2 = 0 suma = 1 n = 2
    cycle = 1
    acc1 = 1  acc2 = 1 suma = 2 n = 3
    cycle = 2
    cycle = 1
    cycle = 0
    acc1 = 1  acc2 = 0 suma = 1 n = 2
    acc1 = 2  acc2 = 1 suma = 3 n = 4
    cycle = 3
    cycle = 2
    cycle = 1
    cycle = 0
    acc1 = 1  acc2 = 0 suma = 1 n = 2
    cycle = 1
    acc1 = 1  acc2 = 1 suma = 2 n = 3
    acc1 = 3  acc2 = 2 suma = 5 n = 5
    The Fibonacci number is: 5

    *)

let rec fib n =
    //printfn "cycle = %d" n
    match n <= 1 with 
    | true -> n
    | false -> 
        let acc1 = fib (n - 1)
        let acc2 = fib (n - 2)
        let suma = acc1 + acc2
        //printfn "acc1 = %d  acc2 = %d suma = %d n = %d" acc1 acc2 suma n
        suma
   
let rec fib1 n =
    match n <= 1 with 
    | true -> n
    | false -> fib (n - 1) + fib (n - 2)

let fibTailRecursive1 n =
    let rec fibIter n a b =
        match n with
        | 0 -> a
        | _ -> fibIter (n - 1) b (a + b)
    fibIter n 0 1


printfn "The Fibonacci number is: %d" <| fib1 10 

printfn "The Fibonacci number Tail1 is: %d" <| fibTailRecursive1 10    

//************************************ Kata2 ****************************************************************

(*
Write function toInitials returs initials for a given person name. E.g: "Bill Gates" -> "B. G."
Note: initials should be separated with a space.
*)

//let toString : char seq -> string = Seq.map string >> String.concat String.Empty

let initials (name:string) =    
    name
    |> Seq.filter (fun item -> System.Char.IsUpper(item))
    |> Seq.map (fun item -> sprintf "%c.%c" <| item <| char 32)
    |> String.concat String.Empty 
    |> Seq.rev |> Seq.tail |> Seq.rev
    |> (Seq.map string >> String.concat String.Empty)

let removeLastCharacter (s: string) =    
    s   
    |> Seq.rev |> Seq.tail |> Seq.rev
    |> (Seq.map string >> String.concat String.Empty)

let removeLastChar (s: string) = s.[0..s.Length-2]

let toInitials (name:string) =    
    name.Split(char 32)
    |> Seq.map (Seq.head >> sprintf "%c.")          
    |> String.concat (string <| char 32)

printfn "Initials: %s|" <| initials "John F. Kennedy"       


//***************************************************** Kata3 *********************************************************************
(*
You probably know the "like" system from Facebook and other pages. People can "like" blog posts, pictures or other items. We want to create the text that should be displayed next to such an item.

Implement the function which takes an array containing the names of people that like an item. It must return the display text as shown in the examples:

[]                                -->  "no one likes this"
["Peter"]                         -->  "Peter likes this"
["Jacob", "Alex"]                 -->  "Jacob and Alex like this"
["Max", "John", "Mark"]           -->  "Max, John and Mark like this"
["Alex", "Jacob", "Mark", "Max"]  -->  "Alex, Jacob and 2 others like this"

*)

let likes (names: string list): string = 
    
    match names with
    | []                      -> "no one likes this"
    | head::[]                -> sprintf "%s likes this" head
    | head1::head2::[]        -> sprintf "%s and %s like this" head1 head2
    | head1::head2::head3::[] -> sprintf "%s, %s and %s like this" head1 head2 head3
    | head1::head2::tail      -> sprintf "%s, %s and %i others like this" head1 head2 tail.Length


let likes1 (names: string list): string = 
       
    match names with
    | [] -> "no one likes this"
    | [x] -> sprintf "%s likes this" x
    | [x; y] -> sprintf "%s and %s like this" x y
    | [x; y; z] -> sprintf "%s, %s and %s like this" x y z
    | x::y::rest -> sprintf "%s, %s and %i others like this" x y rest.Length

printfn "test: %s" (likes List.empty)
printfn "test: %s" (likes ["Peter"]) 
printfn "test: %s" (likes [ "Alex"; "Jacob" ]) 
printfn "test: %s" (likes [ "Max"; "John"; "Mark" ]  ) 
printfn "test: %s" (likes [ "Alex"; "Jacob"; "Mark"; "Max"; "Peter" ]) 

printfn "test: %s" (likes1 ["Peter"]) 
printfn "test: %s" (likes1 [ "Alex"; "Jacob" ]) 
printfn "test: %s" (likes1 [ "Max"; "John"; "Mark" ]  ) 
printfn "test: %s" (likes1 [ "Alex"; "Jacob"; "Mark"; "Max"; "Peter" ]) 


//****************************************** Kata4 *********************************************************************
(*
The rgb function is incomplete. Complete it so that passing in RGB decimal values will result in a hexadecimal representation being returned. Valid decimal values for RGB are 0 - 255. Any values that fall out of that range must be rounded to the closest valid value.

Note: Your answer should always be 6 characters long, the shorthand with 3 will not work here.

Examples (input --> output):
255, 255, 255 --> "FFFFFF"
255, 255, 300 --> "FFFFFF"
0, 0, 0       --> "000000"
148, 0, 211   --> "9400D3"

decimal	Decimal	A floating point data type that has at least 28 significant digits.	1.0m

*)

type RgbConversionError =
    | OptionErr of string
    | ExThrown of string
    | Dummy 

let rgb r g b =

    let mySeq: seq<decimal> = seq { abs r; abs g; abs b }
    let numberOfFractionalDigits = 0
   
    mySeq 
    |> Seq.map (fun item -> 
                          try   
                              let roundedNumber = Math.Round(item, numberOfFractionalDigits) //Microsoft.VisualBasic.Conversion.Hex() should round it automatically, but better to err on the side of caution
                              //failwith "Simulated Error 1"
                              try
                                  //failwith "Simulated Error 2"
                                  Microsoft.VisualBasic.Conversion.Hex(byte roundedNumber)
                                  |> Option.ofObj
                                  |> function                                   
                                      | Some value -> 
                                                    match value.Length with
                                                    | 1 -> Ok <| sprintf "%s%s" "0" value
                                                    | _ -> Ok value
                                      | None       -> 
                                                    Error(OptionErr(".Net Hex Conversion Method Failed"))
                                with
                                | ex -> Error(ExThrown(string ex.Message))
                          with
                          | ex -> Error(ExThrown(string ex.Message))
               )         
    |> Option.ofObj   
    |> function
        | Some value -> 
                      let result = 
                          value |> Seq.map (fun item ->
                                                      match item with
                                                      | Ok value -> value
                                                      | Error _  -> "Dummy"
                                           ) |> String.Concat
                    
                      match result.Contains("Dummy") with
                      | true  -> 
                               let err = 
                                   value |> Seq.map (fun item ->
                                                               match item with
                                                               | Ok _      -> Dummy
                                                               | Error err -> err
                                                    ) |> Seq.head //One exception or None is enough for the calculation to fail
                               Error err
                      | false ->
                               Ok result                        
        | None       -> Error(OptionErr("Hex Conversion Failed"))

let printIt fn = 
    match fn with
    | Ok value  -> printfn "testHexadecimal: %s" value  
    | Error err -> 
                  match err with
                  | OptionErr value -> printfn "testHexadecimal: %s" value
                  | ExThrown value  -> printfn "testHexadecimal: %s" value    
                  | Dummy           -> ()

printIt <| rgb 148m 0m 211.1m

//****************************** Generating random string *********************************************

let [<Literal>] internal apiFormatPattern = @"^[a-z0-9]{32}$"

let validateStringAgainstRegex (input: string) =                
    try
        new Regex(apiFormatPattern)
        |> Option.ofObj 
        |> function
            | Some regex -> regex.IsMatch(input)
            | None       -> false
    with
    | ex -> failwith (sprintf "Error during regex creation: %s" ex.Message)
 
let randomValidString () =   
    try
        let generateRandomString length =
            let random = Random()
            let chars = "abcdefghijklmnopqrstuvwxyz0123456789"             
            Array.init length (fun _ -> chars.[random.Next(chars.Length)])
            |> Array.fold (fun acc c -> sprintf "%s%c" acc c) String.Empty     

        generateRandomString 32
    with
    | ex -> failwith (sprintf "Error during random string creation: %s" ex.Message)

try 
    [1..100] |> List.iter (fun item -> 
                                    let randomValidString = randomValidString() 
                                    match (&&) (validateStringAgainstRegex randomValidString) (not <| System.Object.ReferenceEquals(randomValidString, "0884cac6b05b4dc7bc12ef84a20f09ee")) with  
                                    | true  -> printfn "true: %s" randomValidString                                                                                            
                                    | false -> printfn "false: %s" randomValidString 
                          )
with
| ex -> failwith (sprintf "Error during testing: %s" ex.Message)

//**************************** leetcode.com Task "Two Sum" ******************************************************

(*
Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
You may assume that each input would have exactly one solution, and you may not use the same element twice.
You can return the answer in any order.

2 <= nums.length <= 104
-109 <= nums[i] <= 109
-109 <= target <= 109

*)

let leetCodeM (nums: int array) (target: int) = 
    
    let nums = nums |> List.ofArray |> List.indexed

    nums 
    |> List.choose
        (fun (i, item1) -> 
                         nums 
                         |> List.tryFind (fun (j, item2) -> item1 + item2 = target) 
                         |> Option.map (fun item -> (i, (fst item)))
              
        ) |> List.head

    (*
    
    let leetCode3 (nums: int array, target: int) =
    nums
    |> Array.indexed 
    |> Array.choose
        (fun (i, item1) ->
                        nums
                        |> Array.indexed
                        |> Array.tryFindIndex (fun (_, item2) -> item1 + item2 = target)
                        |> Option.map (fun j -> (i, j))
    )
    |> Array.head    
    *)
   
printfn "leetCodeM %A" <| leetCodeM [|2;7;11;15|] 17

//for C# conversion   
let leetCode ((nums: int array), (target: int)) = 
           
    let nums = nums |> List.ofArray
    
    nums |> List.mapi (fun i item1 -> 

            nums |> List.mapi (fun j item2 -> 
                 
                match i <> j with
                | true  -> 
                         let cond = item2 + item1 = target
                         match cond with
                         | true -> Some (i, j)
                         | false -> None
                | false ->
                         None                      
            )
            |> List.choose (fun item -> item)
                  
    ) |> List.concat |> List.head


(*  

public class Solution {
    public int[] TwoSum(int[] nums, int target)
    {
        List<Tuple<int, int>> indexedList = nums
            .Select((value, index) => Tuple.Create(index, value))
            .ToList();

        var result = indexedList
            .SelectMany(item1 => indexedList
                .Where(item2 => item1.Item1 != item2.Item1 && item1.Item2 + item2.Item2 == target)
                .Select(item2 => new int[] { item1.Item1, item2.Item1 })
            )
            .FirstOrDefault();

        if (result != null)
        {
            return result;
        }

        return new int[0]; // Return an empty array if no valid pair is found
    }
}

public class Solution {
    public int[] TwoSum(int[] nums, int target)
    {
        var pairs = nums
            .SelectMany((item1, i) =>
                nums.Select((item2, j) =>
                {
                    if (i != j)
                    {
                        var cond = item1 + item2 == target;
                        if (cond)
                        {
                            return new int[] { i, j };
                        }
                    }
                    return new int[0];
                }))
            .Where(item => item.Length > 0)
            .ToArray();
    
        if (pairs.Length > 0)
        {
            return pairs[0];
        }
        return new int[0];
    }
}

//suggested best solution
public int[] TwoSum(int[] nums, int target) 
{
    var pairs = new Dictionary<int, int>();
    for(int i = 0; i < nums.Length; i++)
        if(pairs.ContainsKey(target - nums[i]))
            return new int[] { pairs[target - nums[i]], i };
        else
            pairs.TryAdd(nums[i], i);
    
    return default;
}

*)

let twoSum (nums: int array) (target: int) =

    let rec findPairs (map: Map<int, int>) (index: int) (acc: int array) =

        match index < nums.Length with
        | true  -> 
                match Map.tryFind (target - nums.[index]) map with
                | Some i -> 
                          Array.append acc [| i; index |]
                | None   -> 
                         let newMap = Map.add nums.[index] index map
                         findPairs newMap (index + 1) acc
        | false -> 
                acc

    findPairs Map.empty 0 [||]
    
printfn "twoSum %A" <| twoSum [|2;7;11;15|] 17

//*************************************** leetcode.com Task "Add Two Numbers" **************************************************

(*
Input: l1 = [1,4,3], l2 = [1,7,1]; Output: [2,1,5]; Explanation: 341 + 171 = 512

Input: l1 = [2,4,3], l2 = [5,6,4]
Output: [7,0,8]
Explanation: 342 + 465 = 807.

Input: l1 = [0], l2 = [0]
Output: [0]

Example 3:

Input: l1 = [9,9,9,9,9,9,9], l2 = [9,9,9,9]
Output: [8,9,9,9,0,0,0,1]
 
Constraints:
The number of nodes in each linked list is in the range [1, 100].
0 <= Node.val <= 9
It is guaranteed that the list represents a number that does not have leading zeros.
*)

let AddTwoNumbers1 l1 l2 = 

    let reverse (list: int list) =       
        list
        |> List.mapi (fun i item -> int64 item * pown 10L (int i)) // |> List.mapi (fun i item -> int <| item * (int (10.0 ** float i)))
        |> List.fold (+) 0L 
        (*
        list
        |> List.rev 
        |> List.fold (fun acc digit -> int64 (acc * 10L + int64 digit)) 0L 
        *)

    // let reverse list = list |> List.rev |> List.fold (fun acc digit -> acc * 10 + digit) 0 
                  
    let guardRail1 list = 
        let x = reverse list 
        //printfn "Number %i" x
        match x > int64 (Int32.MaxValue) with
        | true  -> printfn "Error2"
                   0L
        | false -> x 

    let sum =        
        let x1 = guardRail1 l1
        let x2 = guardRail1 l2

        let guardRail2 x1 x2 = 
            match (x1 + x2) > int64 (Int32.MaxValue) with
            | true  -> printfn "Error3"
                       0
            | false -> int ((+) x1 x2) 
            
        guardRail2 x1 x2        
           
    //let length = int (log10 (float number)) + 1 //the number of digits in an integer is the floor of the base-10 logarithm of that integer plus 1.
    let n = (int (log10 (float sum)) + 1) - 1

    let rec reverseBack n sum acc = 
        match n < 0 with 
        | true  -> [0]                
        | false -> 
                   let digit = sum / pown 10 n
                   let diff = sum - digit * pown 10 n
                   match diff, n with
                   | 0, 0 -> digit::acc //[ digit ] |> List.append acc
                   | _    -> reverseBack (n - 1) diff (digit::acc)  //([ digit ] |> List.append acc |> List.rev)   
                    
    reverseBack n sum [] 

    (*
    let rec splitDigits acc n =
        match n with
        | 0 -> acc
        | _ ->
               let digit = n % 10
               splitDigits (digit :: acc) (n / 10)
    
    match sum with
    | 0 -> [0]
    | _ -> List.rev (splitDigits [] sum)
    *)


printfn "AddTwoNumbers1 %A" <| AddTwoNumbers1 [1;4;3] [1;7;1]                                  
printfn "AddTwoNumbers1 %A" <| AddTwoNumbers1 [2;4;3] [5;6;4]    
printfn "AddTwoNumbers1 %A" <| AddTwoNumbers1 [9;9;9;9;9;9;9] [9;9;9;9]   
printfn "AddTwoNumbers1 %A" <| AddTwoNumbers1 [0] [0] 
let list1 = ([1..8] |> List.map (fun _ -> Random().Next(2, 9) - 1))
let list2 = ([1..8] |> List.map (fun _ -> Random().Next(2, 7)) |> List.rev) 

printfn "AddTwoNumbers1 %A" <| AddTwoNumbers1 list1 list2  
printfn "%A" <| list1   
printfn "%A" <| list2  

let AddTwoNumbers2 l1 l2 = //for Int32 value only

    let reverse list = list |> List.mapi (fun i item -> item * pown 10 i) |> List.fold (+) 0 
    let sum = (+) (reverse l1) (reverse l2)  //sum -> Int32 value only
    let n = 
        let n = (int (log10 (float sum)) + 1) - 1
        match (>) n 0 with
        | true  -> n
        | false -> 0    

    let rec reverseBack sum acc n =       
        let digit = sum / pown 10 n
        let diff = sum - digit * pown 10 n
        match diff, n with
        | 0, 0 -> digit :: acc 
        | _    -> reverseBack diff (digit :: acc) (n - 1)       
    
    //reverseBack sum [] n 

    let list = [0..(n - 1)]
    let rec reverseBack1 list sum acc n =
        let digit = sum / pown 10 n
        let diff = sum - digit * pown 10 n
        match list with 
        | []        -> digit :: acc
        | _ :: tail -> reverseBack1 tail diff (digit :: acc) (n - 1) 

    reverseBack1 list sum [] n 

let AddTwoNumbers3 l1 l2 = //for Int32 values only/
    let s = (+) (l1 |> List.mapi (fun i x -> x * pown 10 i) |> List.fold (+) 0) (l2 |> List.mapi (fun i x -> x * pown 10 i) |> List.fold (+) 0) 
    let n = match ((int (log10 (float s)) + 1) - 1) > 0 with true -> (int (log10 (float s)) + 1) - 1 | false -> 0
    let rec rb s acc n = match ((s - (s / pown 10 n) * pown 10 n), n) = (0, 0) with true -> (s / pown 10 n) :: acc | false -> rb (s - (s / pown 10 n) * pown 10 n) ((s / pown 10 n) :: acc) (n - 1)    
    rb s [] n 
    
printfn "AddTwoNumbers2 %A" <| AddTwoNumbers2 [1;4;3] [1;7;1]                                  
printfn "AddTwoNumbers2 %A" <| AddTwoNumbers2 [2;4;3] [5;6;4]    
printfn "AddTwoNumbers2 %A" <| AddTwoNumbers2 [9;9;9;9;9;9;9] [9;9;9;9]   
printfn "AddTwoNumbers2 %A" <| AddTwoNumbers2 [0] [0] 
//let list1 = ([1..18] |> List.map (fun _ -> Random().Next(2, 9) - 1))
//let list2 = ([1..18] |> List.map (fun _ -> Random().Next(2, 7)) |> List.rev) 

printfn "AddTwoNumbers2 %A" <| AddTwoNumbers2 list1 list2  
printfn "%A" <| list1   
printfn "%A" <| list2  
            
     
//let result = x * (10.0 ** float n)
//x is the number you want to multiply by 10 raised to the power of n.


//******************************* Free Monad **********************************************



//********************************************************************************************************************

open System
open System.Net.Http
    

//This is an example taken from FSharp Slack
//The solution to this naive variant was mutable as well, so I created an immutable variant below.

let naiveVariant () = 
    let client = new HttpClient()
    let fsharpIssuesUrl = "https://github.com/dotnet/fsharp/issues"
    
    let doesTicketExistAsync ticketNumber = 
        task 
                {
                let uri = Uri $"{fsharpIssuesUrl}/{ticketNumber}"
                let! response = client.GetAsync uri
                return response.IsSuccessStatusCode
                }
    
    let goThroughFsharpTicketsAsync() = 
            
        task
            {
                let mutable ticketNumber = 1
                let mutable keepGoing = true
    
                while keepGoing do
                    match! doesTicketExistAsync ticketNumber with
                    | true ->
                        printfn $"Mutable - found a PR or issue #{ticketNumber}."
                        ticketNumber <- ticketNumber + 1
                    | false ->
                        keepGoing <- false
                        printfn $"#{ticketNumber} is not created yet."
            }
    
    goThroughFsharpTicketsAsync().Wait()

naiveVariant ()   

let immutableVariant2 () =
    let client = new HttpClient()
    let fsharpIssuesUrl = "https://github.com/dotnet/fsharp/issues"
    
    let doesTicketExistAsync ticketNumber =

        async
            {
                let uri = Uri $"{fsharpIssuesUrl}/{ticketNumber}"    
                let! response = client.GetAsync uri |> Async.AwaitTask
                return response.IsSuccessStatusCode
            }
    
    let rec goThroughFsharpTicketsAsync ticketNumber acc =

        async
            {
                match! doesTicketExistAsync ticketNumber with
                | true  ->
                        let _ = ticketNumber :: acc
                        printfn $"Found a PR or issue #{ticketNumber}."
                        return! goThroughFsharpTicketsAsync (ticketNumber + 1) acc
                | false ->
                        printfn $"#{ticketNumber} is not created yet."
                        return List.rev acc  
            }

    goThroughFsharpTicketsAsync 1 [] |> Async.RunSynchronously

    (*
    let rec goThroughFsharpTicketsAsync ticketNumber foundTickets =
        task
            {
                match! doesTicketExistAsync ticketNumber with
                | true ->
                        let updatedTickets = ticketNumber :: foundTickets
                        printfn $"Found a PR or issue #{ticketNumber}."
                        return! goThroughFsharpTicketsAsync (ticketNumber + 1) updatedTickets
                | false ->
                        printfn $"#{ticketNumber} is not created yet."
                        return List.rev foundTickets  // Reverse the list to maintain order
            }
    goThroughFsharpTicketsAsync 1 [] |> Async.AwaitTask
    *)     
immutableVariant2 () |> ignore  




//******************************* Reader Monad **********************************************
// Define a type alias for the reader monad
type Reader<'e, 'a> = 'e -> 'a
         
type ReaderBuilder = ReaderBuilder with
        member __.Bind(m, f) = fun env -> f (m env) env      
        member __.Return(x) = fun _ -> x
        member __.ReturnFrom(x) = x

let reader = ReaderBuilder 
    
// Example: A function that reads an environment value using the Reader CE
let readEnvironment : Reader<int, int> = 
    fun env -> env
    
// Example: A function that uses the environment with the Reader CE
let addWithEnvironment : Reader<int, int> = 
    reader { let! value = readEnvironment in return value + 10 }
    
// Example: Using the functions with the Reader CE
let result = readEnvironment 5   // Result: 5
let result2 = addWithEnvironment 5  // Result: 15
       
    
// Example: A function that reads an environment value
let readEnvironment1: Reader<int, int> = 
    fun env -> env
            
// Example: A function that uses the environment
let addWithEnvironment1: Reader<int, int> = 
    fun env ->
        let value = readEnvironment1 env
        value + 10
    
// Example: Composing functions in the reader monad
let result1 = readEnvironment1 5   // Result: 5
let result21 = addWithEnvironment1 5  // Result: 15


// Example: A function that reads an environment value
let readEnvironment2 (env: int) : int =
    env
   
// Example: A function that uses the environment
let addWithEnvironment2 (env: int) : int =
    let value = readEnvironment2 env
    value + 10
   
// Example: Using the functions
let result22 = readEnvironment2 5   // Result: 5
let result2222 = addWithEnvironment2 5  // Result: 15

//*******************************************************************************************************************

let calculateRectangleArea1 length1 width1 =
    length1 * width1
    
let length1 = 5.0
let width1 = 3.0
let area1 = calculateRectangleArea1 length1 width1
printfn "The area of the rectangle is %.2f" area1
    

let calculateRectangleArea : Reader<(float * float), float> =
    reader {
        let! (length, width) = fun env -> env
        return length * width
    }
    
let length = 5.0
let width = 3.0
let area = calculateRectangleArea (length, width)
printfn "The area of the rectangle is %.2f" area

Console.ReadKey() |> ignore