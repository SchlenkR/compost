#load "../playback.fsx"

open Core
open Blocks
open Playback

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

//
//block {
//    let! x = Osc.sin 2000.0
//    return x * 0.5 } |> playSync 2.5<s>

let fmSynth =
    fun frq ->
        block {
            let amount = 0.05
            let! modulator = Osc.sin 3500.0
            let! s = Osc.sin (frq * (1.0 - modulator * amount))
            return s * 0.5
        }
    |> Synth

let amSynth =
    fun frq ->
        block {
            let amount = 0.5
            let! modulator = Osc.sin (frq * 1.5)
            let! s = Osc.sin frq
            let v = s * 0.5 * ((1.0 - modulator * amount))
            let! lp = Filter.lowPass v { q = 1.0; frq = 4000.0; gain = 1.0 }
            return lp
        }
    |> Synth

//(let (Synth s) = amSynth in s 2000.0 |> playSync 2.5<s>)

let gatedSynth = (makePlayable (Envelope.ar 0.001 0.005 |> Envelope) amSynth) |> Voice

let final =
//    sequencer gatedSynth 120.0 8.0 [ C4; D4; E4; Rel; Rel; A4; B4; C5 ]
    sequencer gatedSynth 120.0 8.0 [ C4; C4; Rel; Rel; Rel; A5; Sus; Sus; Sus; A5 ]
    |> playSync 5.0<s>


//Eval.Test.evalN44k (Envelope.follow 1.0 1.0) 44100 |> List.iteri (fun i x -> printfn "%d - %f" i x)
