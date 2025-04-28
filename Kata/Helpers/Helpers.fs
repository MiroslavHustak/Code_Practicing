namespace Helpers

module Option = 

    let internal ofBool cond = 
        cond                      
        |> function   
            | true  -> Some ()  
            | false -> None

    let internal fromBool value cond : 'a option = 
        cond                      
        |> function   
            | true  -> Some value  
            | false -> None

    let internal ofNull (value: 'nullableValue) =
        match System.Object.ReferenceEquals(value, null) with //The "value" type can be even non-nullable, and the library method will still work.
        | true  -> None
        | false -> Some value

    let internal ofObj value =
        match value with
        | null -> None
        | _    -> Some value

    let internal ofNullable (value: System.Nullable<'T>) =
        match value.HasValue with
        | true  -> Some value.Value
        | false -> None

    let internal toResult err f : Result<'a, 'b> = 
        f                      
        |> function   
            | Some value -> Ok value 
            | None       -> Error err    

module Miscellaneous = 

    open System.IO
    type PureFunction<'a> = 'a
    type PureFunction<'T, 'TError> = Result<'T, 'TError>
    type ImpureFunction<'T, 'TError> = Result<'T, 'TError>
    type ImpureFunction<'b> = 'b    
       
    let impureMultiplyAndPrintWithCustomType x y : ImpureFunction<unit> = printfn "%i" (x * y) 
                  
    let pureAdd a b : PureFunction<int> = a + b
        
    let pureDivide a b : PureFunction<int, string> =
        match b = 0 with
        | true  -> Ok (a / b)
        | false -> Error "Division by zero"

    let impure path : ImpureFunction<int> =

        let x = Path.GetFullPath(path)

        let result = String.length x
        result

    let impureDivide path b : ImpureFunction<int, string> =
        let a = Path.GetFullPath(path).Length
        match b = 0 with
        | true  -> Ok (a / b)
        | false -> Error "Division by zero"