
#load "compost.fsx"
open Compost

let invert () =
    fun s r ->
        let v = not s
        { value = v; state = v }
    |> liftSeed false
    |> Block

// val it : bool list = [true; false; true; false; true; false; true; false; true; false]
Eval.Generator.toReaderSeqWithStateAndValues (fun _ -> ()) (invert())
|> Seq.take 10
|> Seq.toList
