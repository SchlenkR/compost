
#load "./lib/playback.fsx"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Core
open Blocks
open Playback

// 1 - play a simple sin wave for 2.5 seconds
block {
    let! x = Osc.sin 2000.0
    return x * 0.5 
}
|> playSync 2.5<s>
