
#load "../playback.fsx"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Compost
open Blocks
open Playback

block {
    let! x = Osc.sin 2000.0
    return x * 0.5
}
|> playSync 2.5<s>


block {
    let amount = 0.05
    let! fmModulator = Osc.sin 3500.0
    let! s = Osc.tri (1000.0 * (1.0 - fmModulator * amount))
    return s * 0.5
}
|> playSync 2.5<s>

