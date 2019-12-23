open Blocks

#load "../playback.fsx"

open Core
open Blocks
open Playback

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open System

//
//block {
//    let! x = Osc.sin 2000.0
//    return x * 0.5 } |> playSync 2.5<s>

type Synth<'s> = Synth of (float -> Block<float, 's, Env>)

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
            let! modulator = Osc.sin 1500.0
            let! s = Osc.sin frq
            let v = s * 0.5 * ((1.0 - modulator * amount))
            let! lp = Filter.lowPass v { q = 0.2;  frq = 4000.0; gain = 1.0 }
            return lp
        }
    |> Synth

//(let (Synth s) = amSynth in s 2000.0 |> playSync 2.5<s>)

type Envelope<'s> = Envelope of (bool -> Block<float, 's, Env>)

let inline makePlayable (Envelope envelope) (Synth synth) trigger =
    let initialFrq = 0.0
    initialFrq +-> fun lastFrq ->
        block {
            let frq, isTriggered =
                match trigger with
                | None -> lastFrq, false
                | Some frq -> frq, true
            let! s = synth frq
            let! e = envelope isTriggered
            return { out = s * e
                     feedback = frq }
        }

type Voice<'s> = Voice of (float option -> Block<float, 's, Env>)

let gatedSynth = makePlayable (Envelope.ar 1.0 1.0 |> Envelope) amSynth |> Voice

type Step =
    | Key of float
    | Nothing
    | Silence

let sequencer (Voice voice) (bpm: float) (pattern: Step list) =
    let bps = bpm / 60.0
    
    let patternQuant = 4.0

    let initialFrq = 1000.0
    let initials = (0, 1000.0, voice (Some initialFrq))
    
    initials ++> fun s (r: Env) ->
        block {
            let lastQuantIndex, lastFrq, lastVoice = s
            let currentSecs = toSeconds r
            let currentQuantIndex = (Math.Floor(bps * currentSecs * patternQuant) |> int)

            let newQuantIndex, newFrq, newVoice =
                if currentQuantIndex <> lastQuantIndex then
                    let frq = lastFrq + 100.0
                    let voice = voice (Some frq)
                    (currentQuantIndex, frq, voice)
                else
                    (lastQuantIndex, lastFrq, lastVoice)
                    
            let! synthValue = newVoice
            return { out = synthValue
                     feedback = newQuantIndex, newFrq, newVoice }
        }

let final =
    sequencer gatedSynth 60.0 [ Key 1000.0; Key 2000.0; Key 3000.0 ]
    |> playSync 5.0<s>


//Eval.Test.evalN44k (Envelope.follow 1.0 1.0) 44100 |> List.iteri (fun i x -> printfn "%d - %f" i x)
