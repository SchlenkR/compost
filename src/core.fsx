open System


[<AutoOpen>]
module Core =

    type Res<'a, 'b> =
        { value: 'a
          state: 'b }

    type Block<'v, 's, 'r> = Block of ('s option -> 'r -> Res<'v, 's>)

    let runB m =
        match m with
        | Block x -> x

    type Pair<'a, 'b> =
        { mine: 'a
          other: 'b }

    let bindB (m: Block<'a, 'sa, 'r>) (f: 'a -> Block<'b, 'sb, 'r>): Block<'b, Pair<'sa, 'sb>, 'r> =
        let blockFunc localState readerState =
            let unpackedLocalState =
                match localState with
                | None ->
                    { mine = None
                      other = None }
                | Some v ->
                    { mine = Some v.mine
                      other = Some v.other }

            let m' = (runB m) unpackedLocalState.mine readerState
            let fBlock = f m'.value
            let f' = (runB fBlock) unpackedLocalState.other readerState

            { value = f'.value
              state =
                  { mine = m'.state
                    other = f'.state } }

        Block blockFunc

    let returnB x =
        Block(fun _ _ ->
            { value = x
              state = () })

    let returnFromB b = b

    type BlockBuilderBase() =
        member __.Bind(m, f) = bindB m f
        member __.Return x = returnB x
        member __.ReturnFrom l = returnFromB l

    let blockBase = BlockBuilderBase()

    type BlockBuilderGen<'a>() =
        member __.Bind(m: Block<_, _, 'a>, f) = bindB m f
        member __.Return x = returnB x
        member __.ReturnFrom l = returnFromB l

    let blockGen<'a> = BlockBuilderGen<'a>()

    let mapB b f =
        let f' s r =
            let res = runB b s r
            let mappedRes = f res.value
            { value = mappedRes
              state = res.state }
        Block f'

    /// map operator
    let (<!>) = mapB

    let apply (f: Block<'v1 -> 'v2, _, 'r>) (l: Block<'v1, _, 'r>): Block<'v2, _, 'r> =
        blockBase {
            let! l' = l
            let! f' = f
            let result = f' l'
            return result
        }

    /// apply operator        
    let (<*>) = apply

    let inline private binOpBoth left right f =
        blockBase {
            let! l = left
            let! r = right
            return f l r }

    type Block<'v, 's, 'r> with
        static member inline (+) (left, right) = binOpBoth left right (+)
        static member inline (-) (left, right) = binOpBoth left right (-)
        static member inline (*) (left, right) = binOpBoth left right (*)
        static member inline (/) (left, right) = binOpBoth left right (/)
        static member inline (%) (left, right) = binOpBoth left right (%)

    let inline private binOpLeft left right f =
        blockBase {
            let l = left
            let! r = right
            return f l r
        }

    type Block<'v, 's, 'r> with
        static member inline (+) (left, right) = binOpLeft left right (+)
        static member inline (-) (left, right) = binOpLeft left right (-)
        static member inline (*) (left, right) = binOpLeft left right (*)
        static member inline (/) (left, right) = binOpLeft left right (/)
        static member inline (%) (left, right) = binOpLeft left right (%)

    let inline private binOpRight left right f =
        blockBase {
            let! l = left
            let r = right
            return f l r
        }

    type Block<'v, 's, 'r> with
        static member inline (+) (left, right) = binOpRight left right (+)
        static member inline (-) (left, right) = binOpRight left right (-)
        static member inline (*) (left, right) = binOpRight left right (*)
        static member inline (/) (left, right) = binOpRight left right (/)

    /// Reads the global state that is passed around to every loop function.
    let read() =
        Block(fun _ r ->
            { value = r
              state = () })

    /// Lifts a function with an initial value.
    let liftSeed seed block =
        fun s r ->
            let x =
                match s with
                | Some previousState -> previousState
                | None -> seed
            block x r


[<AutoOpen>]
module Feedback =

    [<Struct>]
    type Fbd<'a, 'b> =
        { feedback: 'a
          out: 'b }

    /// Feedback with reader state
    let (++>) seed (f: 'a -> 'r -> Block<Fbd<'a, 'v>, 's, 'r>) =
        let f1 =
            fun prev r ->
                let myPrev, innerPrev =
                    match prev with
                    | None -> seed, None
                    | Some(my, inner) -> my, inner

                let lRes = runB (f myPrev r) innerPrev r
                let feed = lRes.value
                let innerState = lRes.state
                { value = feed.out
                  state = feed.feedback, Some innerState }
        Block f1

    /// Feedback without reader state
    let (+->) seed f = (++>) seed (fun s _ -> f s)


[<AutoOpen>]
module Helper =

    let listN x n =
        x
        |> Seq.take n
        |> Seq.toList


[<AutoOpen>]
module Eval =

    let getValues (s: Res<_, _> seq) = s |> Seq.map (fun x -> x.value)

    let noReader = fun _ -> ()

    module Effect =

        /// Converts a block into a sequence with the given state.
        /// The getReaderState function is called for each evaluation.
        let toSeqSV getReaderState (blockWithInput: 'inp -> Block<_, _, _>) =
            let mutable lastState: 'a option = None
            fun inputValues ->
                inputValues
                |> Seq.mapi (fun i v ->
                    let block = blockWithInput v |> runB
                    let res = block lastState (getReaderState i)
                    lastState <- Some res.state
                    res)

        /// Converts a block into a sequence with the given state.
        /// The getReaderState function is called for each evaluation.
        let toSeqV getReaderState (blockWithInput: 'inp -> Block<_, _, _>) =
            fun inputValues -> toSeqSV getReaderState blockWithInput inputValues |> getValues

    module Generator =

        /// Converts a block into a sequence with the given state.
        /// The getReaderState function is called for each evaluation.
        let toSeqSV getReaderState (blockWithInput: Block<_, _, _>) =
            Effect.toSeqSV getReaderState (fun () -> blockWithInput) (Seq.initInfinite (fun _ -> ())) |> getValues

        /// Converts a block into a sequence with the given state.
        /// The getReaderState function is called for each evaluation.
        let toSeqV getReaderState (blockWithInput: Block<_, _, _>) =
            toSeqSV getReaderState blockWithInput |> getValues

    module Test =
        let evalN block =
            Generator.toSeqSV noReader block |> listN


[<AutoOpen>]
module Audio =

    type Env =
        { samplePos: int
          sampleRate: int }

    let toSeconds env = (double env.samplePos) / (double env.sampleRate)

    let block = BlockBuilderGen<Env>()

    module Eval =

        /// Converts a block and a given sample rate to a sequence.
        let toAudioSeq (b: Block<_, _, Env>) sampleRate =
            b
            |> Eval.Generator.toSeqSV (fun i ->
                { samplePos = i
                  sampleRate = sampleRate })

        /// Converts a block with a sample rate of 44.1kHz to a sequence.
        let toAudioSeq44k (b: Block<_, _, _>) = toAudioSeq b 44100

        module Test =

            let evalN sr block =
                toAudioSeq block sr |> listN

            let evalN44k block =
                toAudioSeq44k block |> listN


[<AutoOpen>]
module Compose =

    type Trigger =
        | Hold of float
        | Sus
        | Rel

    type Synth<'s> = Synth of (float -> Block<float, 's, Env>)

    type Envelope<'s> = Envelope of (bool -> bool -> Block<float, 's, Env>)

    type Voice<'s> = Voice of (float option -> bool -> Block<float, 's, Env>)

    let inline makePlayable (Envelope envelope) (Synth synth) trigger resetTrigger =
        let initialFrq = 0.0
        initialFrq +-> fun lastFrq ->
            block {
                let frq, isTriggered =
                    match trigger with
                    | None -> lastFrq, false
                    | Some frq -> frq, true
                let! s = synth frq
                let! e = envelope isTriggered resetTrigger
                return { out = s * e
                         feedback = frq }
            }

    let sequencer (Voice voice) (bpm: float) beats (pattern: Trigger list) =

        let index l i =
            let length = l |> List.length
            let i' = i - (i / length * length)
            l.[i']

        let bps = bpm / 60.0
        let patternQuant = beats / 4.0
        let initials = (-1, 1000.0)

        initials ++> fun s (r: Env) ->
            block {
                let lastQuantIndex, lastFrq = s
                let currentSecs = toSeconds r
                let currentQuantIndex = (Math.Floor(bps * currentSecs * patternQuant) |> int)

                let beatChanged = currentQuantIndex <> lastQuantIndex

                let newQuantIndex, newFrq, trigger, resetTrigger =
                    let step = index pattern currentQuantIndex

                    match step with
                    | Hold frq -> currentQuantIndex, frq, Some frq, beatChanged
                    | Sus -> lastQuantIndex, lastFrq, Some lastFrq, false
                    | Rel -> lastQuantIndex, lastFrq, None, false

                let! synthValue = voice trigger resetTrigger
                return { out = synthValue
                         feedback = newQuantIndex, newFrq }
            }

    [<AutoOpen>]
    module Notes =

        let C0 = Hold 16.351597831287414
        let Cs0 = Hold 17.323914436054505
        let D0 = Hold 18.354047994837977
        let Ds0 = Hold 19.445436482630058
        let E0 = Hold 20.601722307054366
        let F0 = Hold 21.826764464562746
        let Fs0 = Hold 23.12465141947715
        let G0 = Hold 24.499714748859326
        let Gs0 = Hold 25.956543598746574
        let A0 = Hold 27.5
        let As0 = Hold 29.13523509488062
        let B0 = Hold 30.86770632850775

        let C1 = Hold 32.70319566257483
        let Cs1 = Hold 34.64782887210901
        let D1 = Hold 36.70809598967594
        let Ds1 = Hold 38.890872965260115
        let E1 = Hold 41.20344461410875
        let F1 = Hold 43.653528929125486
        let Fs1 = Hold 46.2493028389543
        let G1 = Hold 48.999429497718666
        let Gs1 = Hold 51.91308719749314
        let A1 = Hold 55.0
        let As1 = Hold 58.27047018976124
        let B1 = Hold 61.7354126570155

        let C2 = Hold 65.40639132514966
        let Cs2 = Hold 69.29565774421802
        let D2 = Hold 73.41619197935188
        let Ds2 = Hold 77.78174593052023
        let E2 = Hold 82.4068892282175
        let F2 = Hold 87.30705785825097
        let Fs2 = Hold 92.4986056779086
        let G2 = Hold 97.99885899543733
        let Gs2 = Hold 103.82617439498628
        let A2 = Hold 110.0
        let As2 = Hold 116.54094037952248
        let B2 = Hold 123.47082531403103

        let C3 = Hold 130.8127826502993
        let Cs3 = Hold 138.59131548843604
        let D3 = Hold 146.8323839587038
        let Ds3 = Hold 155.56349186104046
        let E3 = Hold 164.81377845643496
        let F3 = Hold 174.61411571650194
        let Fs3 = Hold 184.9972113558172
        let G3 = Hold 195.99771799087463
        let Gs3 = Hold 207.65234878997256
        let A3 = Hold 220.0
        let As3 = Hold 233.08188075904496
        let B3 = Hold 246.94165062806206

        let C4 = Hold 261.6255653005986
        let Cs4 = Hold 277.1826309768721
        let D4 = Hold 293.6647679174076
        let Ds4 = Hold 311.1269837220809
        let E4 = Hold 329.6275569128699
        let F4 = Hold 349.2282314330039
        let Fs4 = Hold 369.9944227116344
        let G4 = Hold 391.99543598174927
        let Gs4 = Hold 415.3046975799451
        let A4 = Hold 440.0
        let As4 = Hold 466.1637615180899
        let B4 = Hold 493.8833012561241

        let C5 = Hold 523.2511306011972
        let Cs5 = Hold 554.3652619537442
        let D5 = Hold 587.3295358348151
        let Ds5 = Hold 622.2539674441618
        let E5 = Hold 659.2551138257398
        let F5 = Hold 698.4564628660078
        let Fs5 = Hold 739.9888454232688
        let G5 = Hold 783.9908719634985
        let Gs5 = Hold 830.6093951598903
        let A5 = Hold 880.0
        let As5 = Hold 932.3275230361799
        let B5 = Hold 987.7666025122483

        let C6 = Hold 1046.5022612023945
        let Cs6 = Hold 1108.7305239074883
        let D6 = Hold 1174.6590716696303
        let Ds6 = Hold 1244.5079348883237
        let E6 = Hold 1318.5102276514797
        let F6 = Hold 1396.9129257320155
        let Fs6 = Hold 1479.9776908465376
        let G6 = Hold 1567.981743926997
        let Gs6 = Hold 1661.2187903197805
        let A6 = Hold 1760.0
        let As6 = Hold 1864.6550460723597
        let B6 = Hold 1975.533205024496

        let C7 = Hold 2093.004522404789
        let Cs7 = Hold 2217.4610478149766
        let D7 = Hold 2349.31814333926
        let Ds7 = Hold 2489.0158697766474
        let E7 = Hold 2637.02045530296
        let F7 = Hold 2793.825851464031
        let Fs7 = Hold 2959.955381693075
        let G7 = Hold 3135.9634878539946
        let Gs7 = Hold 3322.437580639561
        let A7 = Hold 3520.0
        let As7 = Hold 3729.3100921447194
        let B7 = Hold 3951.066410048992
