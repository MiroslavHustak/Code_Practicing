namespace MachineLearning

open System

//open MathNet.Numerics.FSharp

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

open MathNet.Numerics.Distributions

module MachineLearning = 

    let private tolerance = 1e-4
    let private learningRate = 0.01
    let private iterations = 100000
    let private samples = 10000
  
    type private Increment =
        | UpdateState of int  
        | CheckState  of AsyncReplyChannel<int>

    let private reportProgress (currentProgress, increment) =  currentProgress + increment

    let private actor =
        MailboxProcessor<Increment>
            .StartImmediate
                <|
                fun inbox 
                    ->
                    let rec loop n = 
                        async
                            { 
                               match! inbox.Receive() with
                               | UpdateState i 
                                   ->
                                   let updated = reportProgress (n, i)
                                   return! loop updated

                               | CheckState  replyChannel 
                                   ->
                                   replyChannel.Reply n
                                   return! loop n
                            }
                    loop 0     
        
    let machineLearningArray () =

        //let mutable counter = 0

        // Generate synthetic data: y = 2 * x1 + 3 * x2 + noise
        let generateMockData (numSamples : int) =

            let rand = System.Random 42
            let noise = Normal(0.0, 0.1) // Gaussian noise //v ML staci takto maly noise, pri linearni regresi v realnem svete muze byt daleko vetsi
              
            Array.init numSamples
                (fun _ 
                    ->
                    let x1 = rand.NextDouble() * 10.0
                    let x2 = rand.NextDouble() * 10.0
                    let noiseSample = noise.Sample()
                    let y = 2.0 * x1 + 3.0 * x2 + noiseSample
                    ([|1.0; x1; x2|], y) // Include bias term (1.0)
                )           
        
        // metoda nejmensich ctvercu (least squares method, squared error) method
        let meanSquaredError (X : Matrix<float>) (y : Vector<float>) (theta : Vector<float>) =

            let predictions = X * theta
            let errors = predictions - y
            (errors.PointwisePower 2).Sum() / (2.0 * float X.RowCount) //mean
         
        (*
        let gradientDescent3 (X : Matrix<float>) (y : Vector<float>) (theta : Vector<float>) (alpha : float) (iterations : int) =

            let m = float X.RowCount

            [ 0 .. iterations - 1 ]
            |> List.fold 
                (fun currentTheta _
                    ->
                    let predictions : Vector<float> = X * currentTheta
                    let errors = predictions - y
                    let gradient = (X.Transpose() * errors) / m
                    currentTheta - alpha * gradient
                ) theta
        *) 
        (*
        It just mechanically updates theta using the gradient.           
        It does not check if the model is getting better or worse.           
        It does not stop early based on precision; it just runs for exactly iterations rounds.   
        *)     
        
        let gradientDescent (X : Matrix<float>) (y : Vector<float>) (theta : Vector<float>) (alpha : float) (maxIterations : int) =
            
            let m = float X.RowCount
                    
            let rec loop currentTheta previousCost iteration =
            
                match iteration >= maxIterations with
                | true
                    ->
                    //counter <- counter + 1
                    actor.Post <| UpdateState 1
                    currentTheta
                | false 
                    ->
                    //counter <- counter + 1
                    actor.Post <| UpdateState 1
                    let predictions : Vector<float> = X * currentTheta
                    let errors = predictions - y
                    let gradient = (X.Transpose() * errors) / m
                    let newTheta = currentTheta - alpha * gradient
                    let newCost = meanSquaredError X y newTheta
                    
                    match abs (previousCost - newCost) < tolerance with // tolerance for normal use: 1e-4 
                    | true  -> newTheta
                    | false -> loop newTheta newCost (iteration + 1)
                    
            loop theta (meanSquaredError X y theta) 0     

        // Generate mock data samples
        let data = generateMockData samples //10000 samples

        let X = 
            DenseMatrix.ofRowArrays (
                Array.init data.Length 
                    (fun i -> fst (data |> Array.item i))
            )
        
        let y = 
            DenseVector.ofArray (
                Array.init data.Length
                    (fun i -> snd (data |> Array.item i))
            )
        
        // Initialize parameters
        let initialTheta = DenseVector.zero 3 // [bias; weight1; weight2]
              
        // Train the model
        let theta : Vector<float> = gradientDescent X y initialTheta learningRate iterations 
        
        // Print results
        printfn "Learned parameters: %A" theta
        printfn "Counter: %i"  <| actor.PostAndReply (fun replyChannel -> CheckState replyChannel)
        //printfn "Counter: %i" counter  
        printfn "Final cost: %f" (meanSquaredError X y theta)
        
        // Make a prediction for a new input [1.0; 5.0; 5.0]
        let newInput = DenseVector.ofArray [|1.0; 5.0; 5.0|]
        let prediction = newInput * theta
        printfn "Prediction for input [1.0; 5.0; 5.0]: %f" prediction    

    let machineLearningList () =
           
        let mutable counter = 0
    
        // Generate synthetic data: y = 2 * x1 + 3 * x2 + noise
        let generateMockData (numSamples : int) =
    
            let rand = System.Random 42
            let noise = Normal(0.0, 0.1) // Gaussian noise //v ML staci takto maly noise, pri linearni regresi v realnem svete muze byt daleko vetsi
                  
            List.init numSamples
                (fun _ 
                    ->
                    let x1 = rand.NextDouble() * 10.0
                    let x2 = rand.NextDouble() * 10.0
                    let noiseSample = noise.Sample()
                    let y = 2.0 * x1 + 3.0 * x2 + noiseSample
                    ([1.0; x1; x2], y) // Include bias term (1.0)
                )           
           
        let meanSquaredError (X : Matrix<float>) (y : Vector<float>) (theta : Vector<float>) =

            let predictions = X * theta
            let errors = predictions - y
            (errors.PointwisePower 2).Sum() / (2.0 * float X.RowCount) //mean
              
        let gradientDescent (X : Matrix<float>) (y : Vector<float>) (theta : Vector<float>) (alpha : float) (maxIterations : int) =
            
            let m = float X.RowCount
                    
            let rec loop currentTheta previousCost iteration =
            
                match iteration >= maxIterations with
                | true
                    ->
                    //counter <- counter + 1
                    actor.Post <| UpdateState 1
                    currentTheta
                | false 
                    ->
                    //counter <- counter + 1
                    actor.Post <| UpdateState 1
                    let predictions : Vector<float> = X * currentTheta
                    let errors = predictions - y
                    let gradient = (X.Transpose() * errors) / m
                    let newTheta = currentTheta - alpha * gradient
                    let newCost = meanSquaredError X y newTheta
                    
                    match abs (previousCost - newCost) < tolerance with // tolerance for normal use: 1e-4 
                    | true  -> newTheta
                    | false -> loop newTheta newCost (iteration + 1)
                    
            loop theta (meanSquaredError X y theta) 0     
    
        let data = generateMockData samples //10000 samples
    
        let X = 
            DenseMatrix.ofRowList (
                List.init data.Length (fun i -> fst (data |> List.item i))
            )
            
        let y = 
            DenseVector.ofList(
                List.init data.Length (fun i -> snd (data |> List.item i))
            )
            
        let initialTheta = DenseVector.zero 3 // [bias; weight1; weight2]
           
        let theta = gradientDescent X y initialTheta learningRate iterations
            
        // Print results
        printfn "Learned parameters: %A" theta
        printfn "Counter: %i"  <| actor.PostAndReply (fun replyChannel -> CheckState replyChannel)
        //printfn "Counter: %i" counter
        printfn "Final cost: %f" (meanSquaredError X y theta)
            
        // Make a prediction for a new input [1.0; 5.0; 5.0]
        let newInput = DenseVector.ofList [1.0; 5.0; 5.0]
        let prediction = newInput * theta
        printfn "Prediction for input [1.0; 5.0; 5.0]: %f" prediction

    //********************************************************************************

    let solveLinearSystem () =

        let A1 = matrix [| [| 2.0; 3.0 |]
                           [| 4.0; -1.0 |] |]
    
        // Constant vector b
        let b1 = DenseVector.ofArray [| 8.0; 7.0 |]

        let m = 
            matrix [[ 1.0; 2.0 ]
                    [ 3.0; 4.0 ]]
        
        let m' = m.Inverse()
        
        // Coefficient matrix A
        let A = 
            matrix [[ 2.0; 3.0 ]
                    [ 4.0; -1.0 ]]
        
        // Constant vector b
        let b = DenseVector.ofList [ 8.0; 7.0 ]
        
        // Solve x = A^(-1) * b
        let x = A.Inverse() * b
        
        // Print the solution
        printfn "Solution x: %A" x

        printfn "Solution: x = %f, y = %f" x.[0] x.[1]
        
        // Verify by computing A * x
        let verification = A * x
        printfn "Verification (should be close to [8; 7]): %A" verification

        //************************************************************************