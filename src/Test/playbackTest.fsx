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

let amSynth2 =
    fun frq ->
        block {
            let amount = 0.5
            let! modulator = 1.0 - (Osc.sin (frq * 1.5)) * amount
            let! v = (Osc.sin frq) * 0.5 * modulator
            let! lp = Filter.lowPass v { q = 1.0; frq = 4000.0; gain = 1.0 }
            return lp
        }

//(let (Synth s) = amSynth in s 2000.0 |> playSync 2.5<s>)



let gatedSynth =
    let envelope = Envelope.ar 0.005 0.0001 |> Envelope
    let synth = amSynth2 |> Synth
    buildVoice envelope synth |> Voice

let jingleBells = [
    
    e5; Sus;   e5; Sus;   e5; Sus;   Rel; Rel
    e5; Sus;   e5; Sus;   e5; Sus;   Rel; Rel
    
    e5; Sus;   g5; Sus;   c5; Sus;   Rel; d5
    e5; Sus;   Sus; Sus;  Sus; Sus;  Rel; Rel
    
    f5; Sus;   f5; Sus;   f5; Sus;   Rel; f5
    f5; Rel;   e5; Rel;   e5; Rel;   e5; e5
    
    g5; Rel;   g5; Rel;   f5; Sus;   d5; Sus
    c5; Sus;   Sus; Sus;  Sus; Sus;  Rel; Rel
    
    Rel; Rel;  Rel; Rel;  Rel; Rel;  Rel; Rel
]

sequencer gatedSynth 90.0 16.0 jingleBells 
|> playSync 12.0<s>



// TODO: Pattern should tell when it's "done" instead of specifying seconds to play
// Crackle: There is a hard "release" in the envelope that has to be fixed


//Eval.Test.evalN44k (Envelope.follow 1.0 1.0) 44100 |> List.iteri (fun i x -> printfn "%d - %f" i x)
