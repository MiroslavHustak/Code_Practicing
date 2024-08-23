module HtmlUtilities

//open System.Reflection

open FSharp.Data

open System.Net
open FSharp.Data.HttpRequestHeaders
open System.IO
open System

open System.Text.Encodings

let main () = 
    //let url = @"https://www.mdpo.cz/f-a-q"
    let url = @"http://nutricniterapie.somee.com/#sluzby/1"

    let document = FSharp.Data.HtmlDocument.Load(url)

    printfn "%A" <| document


    let result = 
        document.Descendants "div"
        |> Seq.collect (fun divNode ->
            match divNode.HasAttribute("class", "sppb-addon-content") with //or HasClass("sppb-addon-content")
            | true ->                
                    divNode.Descendants "p" 
                    |> Seq.map (fun pNode ->
                        //printfn "%s" <| pNode.InnerText()
                        pNode.InnerText()
                    )
            | _   ->
                   Seq.empty
        )
    //result        
    printfn "length %i" <| (result |> Seq.length)
    //printfn "%s" <| (result |> Seq.head)
               
    

