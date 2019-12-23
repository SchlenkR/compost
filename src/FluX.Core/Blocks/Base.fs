module FluX.Blocks.Base

open FluX.Core
open FluX.Audio

/// Delays a given value by 1 cycle.
let delay seed current =
    let f p r =
        { value = p
          state = current }
    f
    |> liftSeed seed
    |> Block

// let delayBy seed samples =
//     fun p _ =

let flp seed input =
    let f p r =
        let res =
            match p, input with
            | false, true -> 1.0
            | _ -> 0.0
        { value = res
          state = input }
    f
    |> liftSeed seed
    |> Block

let fln seed input =
    let f p r =
        let res =
            match p, input with
            | true, false -> 1.0
            | _ -> 0.0
        { value = res
          state = input }
    f
    |> liftSeed seed
    |> Block

let preserve factory =
    let f p r =
        let instance =
            match p with
            | None -> factory()
            | Some x -> x
        { value = instance
          state = instance }
    Block f

let counter (seed: float) (inc: float) =
    let f p r =
        let res = p + inc
        { value = res
          state = res }
    f
    |> liftSeed seed
    |> Block

// TODO
let counterAlt (seed: float) (inc: float) =
    seed <=> fun last _ ->
        loop {
            let value = last + inc
            return { out = value
                     feedback = value }
        }
        
// let toggle seed =
//     let f p _ =
//         match p with
//         | true -> {value=0.0; state=false}
//         | false -> {value=1.0; state=true}
//     f |> liftSeed seed |> L
