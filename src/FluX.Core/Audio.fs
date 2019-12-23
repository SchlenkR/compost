
module FluX.Audio

open FluX.Core

type Env =
    { samplePos: int
      sampleRate: int }

// let toSeconds (env:Env) = (env.samplePos / env.sampleRate) * 1.0<s>

let loop = LoopGenBuilder<Env>()
