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

module CopyingOrMovingFiles =    

    open System.IO
    open System

    [<Struct>]
    type private Builder2 = Builder2 with    
        member _.Bind((optionExpr, err), nextFunc) =
            match optionExpr with
            | Some value -> nextFunc value 
            | _          -> err  
        member _.Return x : 'a = x

    let private pyramidOfDoom = Builder2

    type private CommandLineInstruction<'a> =
        | SourceFilepath of (string -> 'a)
        | DestinFilepath of (string -> 'a)
        | CopyOrMove of (string * string) * 'a

    type private CommandLineProgram<'a> =
        | Pure of 'a 
        | Free of CommandLineInstruction<CommandLineProgram<'a>>

    let private mapI f = 
        function
        | SourceFilepath next  -> SourceFilepath (next >> f)
        | DestinFilepath next  -> DestinFilepath (next >> f)
        | CopyOrMove (s, next) -> CopyOrMove (s, next |> f)    

    let rec private bind f = 
        function
        | Free x -> x |> mapI (bind f) |> Free
        | Pure x -> f x

    type private CommandLineProgramBuilder = CommandLineProgramBuilder with
        member this.Bind(p, f) = //x |> mapI (bind f) |> Free
            match p with
            | Pure x     -> f x
            | Free instr -> Free (mapI (fun p' -> this.Bind(p', f)) instr)
        member this.Return x = Pure x
        member this.ReturnFrom p = p

    let private cmdBuilder = CommandLineProgramBuilder  
    
    type private Config =
        {
            source: string
            destination: string
            fileName: string
        }

    let rec private interpret config msg result f = 

        let source = config.source
        let destination = config.destination
       
        function
        | Pure x -> x
        | Free (SourceFilepath next)  ->
                                      let sourceFilepath source =                                        
                                          pyramidOfDoom
                                             {
                                                 let! value = Path.GetFullPath(source) |> Option.ofNull, Error <| msg "č.2"   
                                                 let! value = 
                                                     (
                                                         let fInfodat: FileInfo = new FileInfo(value)   
                                                         Option.fromBool value fInfodat.Exists
                                                     ), Error <| msg "č.1"
                                                 return Ok value
                                             }      
                                      next (result (sourceFilepath source) source) |> interpret config msg result f
        | Free (DestinFilepath next)  ->
                                      let destinFilepath destination =                                        
                                          pyramidOfDoom
                                             {
                                                 let! value = Path.GetFullPath(destination) |> Option.ofNull, Error <| msg "č.4"   
                                                 let! value = 
                                                     (
                                                         let dInfodat: DirectoryInfo = new DirectoryInfo(value)   
                                                         Option.fromBool value dInfodat.Exists
                                                     ), Error <| msg "č.3"
                                                 return Ok value
                                             }                                        
                                      next (result (destinFilepath destination) destination) |> interpret config msg result f
        | Free (CopyOrMove (s, _)) -> 
                                      let sourceFilepath = fst s
                                      let destinFilepath = snd s  
                                      let resultInt = 
                                          f sourceFilepath destinFilepath 
                                          let x = 42 //simulation of a hypothetical integer result of the previous function
                                          x 
                                      resultInt  //next |> interpret config 

    let private config = 
        {
            source = @"e:\UVstarterLog\log.txt" //kontrola s FileInfo
            destination = @"e:\UVstarterLog\test\" //kontrola s DirectoryInfo
            fileName = "test.txt"
        }

    let private msg = sprintf "Chyba %s při čtení cesty " 

    let private result path1 path2 = 
        match path1 with
        | Ok path1  -> 
                    path1
        | Error err -> 
                    printf "%s%s" err path2 
                    Console.ReadKey() |> ignore 
                    System.Environment.Exit(1) 
                    String.Empty
   
    let private fc = (fun p1 p2 -> File.Copy(p1, p2, true)) //(fun _ _ -> ())
    let private fm = (fun p1 p2 -> File.Move(p1, p2, true)) //(fun _ _ -> ())

    let private copyOrMoveFiles config msg result f =
        
        cmdBuilder 
            {
                let! sourceFilepath = Free (SourceFilepath Pure)                
                let! destinFilepath = Free (DestinFilepath Pure) 
                return! Free (CopyOrMove ((sourceFilepath, sprintf "%s%s" (destinFilepath) config.fileName), Pure 0))
            } |> interpret config msg result f

    let copyFiles () = copyOrMoveFiles config msg result fc
    let moveFiles () = copyOrMoveFiles config msg result fm
   
    