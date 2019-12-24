
#load "lib/core.fsx"
open Core

(* 1st example *)

// a simple block that toggles ongoing from true to false with each evaluation
let invertGenerator seed =
    fun s r ->
        let v = not s
        { value = v; state = v }
    |> liftSeed seed
    |> Block

Core.Eval.Test.evalN (invertGenerator false) 10
// val it : bool list =
//     [true; false; true; false; true; false; true; false; true; false]



(* 2nd example *)
let countEffect =
    fun s r ->
        { value = s; state = s + 1 }
    |> liftSeed 0
    |> Block

// goal: count n values, then mute.
let countUntil n =
    blockBase {
        let! value = invertGenerator false
        let! count = countEffect
        let output =
            if count < n then
                match value with
                | true -> "Oh Yeah!"
                | false -> "Oh no."
            else
                "That's it."
        return output
    }
    
Core.Eval.Test.evalN (countUntil 5) 10
// val it : string list =
//     ["Oh Yeah!"; "Oh no."; "Oh Yeah!"; "Oh no."; "Oh Yeah!"; "That's it.";
//      "That's it."; "That's it."; "That's it."; "That's it."]