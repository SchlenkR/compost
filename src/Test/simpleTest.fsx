
#load "../compost.fsx"
open Compost

// a simple block that toggles ongoing from true to false with each evaluation
let invert =
    fun s r ->
        let v = not s
        { value = v; state = v }
    |> liftSeed false
    |> Block

// val it : bool list = [true; false; true; false; true; false; true; false; true; false]
Eval.Test.evalN invert 10
