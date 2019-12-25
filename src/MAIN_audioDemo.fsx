
#load "./lib/playback.fsx"

open Core
open Blocks
open Playback
open Compose
open Notes

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols


// 2 - a frequency modulated synth
let fmSynth frq =
    block {
        let amount = 0.05
        let! modulator = Osc.sin 3500.0
        let! s = Osc.sin (frq * (1.0 - modulator * amount))
        return s * 0.5
    }

// 3 - an amplitude modulated synth (alternative 1)
let amSynth frq =
    block {
        let amount = 0.5
        let! modulator = Osc.sin (frq * 1.5)
        let! s = Osc.sin frq
        let v = s * 0.5 * (1.0 - modulator * amount)
        let! lp = Filter.lowPass { q = 1.0; frq = 4000.0; gain = 1.0 } v
        return lp
    }

// 4 - an amplitude modulated synth (alternative 2)
let amSynth2 frq =
    block {
        let amount = 0.5
        let! modulator = 1.0 - Osc.sin (frq * 1.5) * amount
        return!
            Osc.sin frq * 0.5 * modulator
            |=> Filter.lowPass { q = 1.0; frq = 4000.0; gain = 1.0 }
    }


// play the am synth for 2.5 seconds
////amSynth2 2000.0 |> playSync 2.5<s>


// 5 - create somthing "triggerable" by combining the am synth
//     and an attack-release envelope
let synthVoice =
    let envelope = Envelope.ar 0.005 0.0001 |> Envelope
    let synth = amSynth2 |> Synth
    buildVoice envelope synth

// 6 - define a melody (no chords; just monophone notes)
let jingleBells = [
    
    e5; Sus;   e5; Sus;   e5; Sus;   Rel; Rel
    e5; Sus;   e5; Sus;   e5; Sus;   Rel; Rel
    
    e5; Sus;   g5; Sus;   c5; Sus;   Rel; d5
    e5; Sus;   Sus; Sus;  Rel; Rel;  Rel; Rel
    
    f5; Sus;   f5; Sus;   f5; Sus;   Rel; f5
    f5; Rel;   e5; Rel;   e5; Rel;   e5; e5
    
    g5; Rel;   g5; Rel;   f5; Sus;   d5; Sus
    c5; Sus;   Sus; Sus;  Sus; Rel;  Rel; Rel
    
    Rel; Rel;  Rel; Rel;  Rel; Rel;  Rel; Rel
]

// 7 - play the synth at 90 BPM. The pattern describes 16th notes
sequencer synthVoice 90.0 16.0 jingleBells 
|> playSync 12.0<s>
