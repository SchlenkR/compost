
#load "compost.fsx"
open Compost

let invert () =
    fun s r ->
        let v = not s
        { value = v; state = v }
    |> liftSeed false
    |> Block

Eval.toSeq (invert())
|> Seq.take 10

