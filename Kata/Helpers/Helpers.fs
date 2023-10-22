module CopyingOrMovingFiles    

open System.IO
open System

let inline internal toGenerics (printError: Lazy<unit>) (gen: 'a) value = 
    value
    |> Option.ofObj
    |> function 
        | Some value ->
                        value
        | None       -> 
                        printError.Force() 
                        gen  
    
let private processFile source destination action =
    let sourceFilepath =
        Path.GetFullPath(source)
        |> toGenerics (lazy (printfn "Chyba při čtení cesty k souboru")) String.Empty 
    let destinFilepath =
        Path.GetFullPath(destination) 
        |> toGenerics (lazy (printfn "Chyba při čtení cesty k souboru")) String.Empty                 
    let fInfodat: FileInfo = new FileInfo(sourceFilepath)  

    match fInfodat.Exists with 
    | true  -> action sourceFilepath destinFilepath
    | false -> printfn "Soubor %s nenalezen" source  
        
//to be wrapped in a tryWith block
//not used yet
let internal copyFiles source destination =
    let action sourceFilepath destinFilepath = File.Copy(sourceFilepath, destinFilepath, true)                
    processFile source destination action
    42
            
//to be wrapped in a tryWith block
//not used yet
let internal moveFiles source destination =
    let action sourceFilepath destinFilepath = File.Move(sourceFilepath, destinFilepath, true)                
    processFile source destination action
    22

