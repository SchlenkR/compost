
#load "../playback.fsx"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Compost
open Blocks
open Playback


block {
    let! x = Osc.sin 2000.0
    return x * 0.5 } |> playSync 2.5<s>


let amSynth frq =
    block {
        let amount = 0.05
        let! fmModulator = Osc.sin 3500.0
        let! s = Osc.tri (frq * (1.0 - fmModulator * amount))
        return s * 0.5
    }


let inline makePlayable (envelope: bool -> Block<_, _, _>) synth trigger =
    let initialFrq = 2000.0
    initialFrq +-> fun lastFrq ->
        block {
            let frq,isTriggered =
                match trigger with
                | None -> lastFrq,false
                | Some frq -> frq,true
            let! s = synth frq
            let! e = envelope isTriggered
            return  { out = s * e
                      feedback = frq }
        }


let playable = makePlayable (Envelope.ar 1.0 1.0) amSynth

playable (Some 2000.0)
|> playSync 5.0<s>


Eval.Test.evalN44k (Envelope.follow 1.0 1.0) 44100 |> List.iteri (fun i x -> printfn "%d - %f" i x)
