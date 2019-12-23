
open System

module Const =
    let pi = Math.PI
    let pi2 = 2.0 * pi
    let sqrt2 = 1.4142135623730951


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


[<AutoOpen>]
module Helper =

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
module AudioEnvironment =

    type Env =
        { samplePos: int
          sampleRate: int }

    let toSeconds env = env.samplePos / env.sampleRate

    let block = BlockBuilderGen<Env>()


[<AutoOpen>]
module Eval =
    
    let getValues (s: Res<_, _> seq) = s |> Seq.map (fun x -> x.value)
    
    module Effect =

        /// Converts a block into a sequence with the given state.
        /// The getReaderState function is called for each evaluation.
        let toReaderSeqWithStateAndValues getReaderState (blockWithInput: 'inp -> Block<_, _, _>) =
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
        let toReaderSeqWithValues getReaderState (blockWithInput: 'inp -> Block<_, _, _>) =
            fun inputValues ->
                toReaderSeqWithStateAndValues getReaderState blockWithInput inputValues
                |> Seq.map (fun x -> x.value)

    
    module Generator =

        /// Converts a block into a sequence with the given state.
        /// The getReaderState function is called for each evaluation.
        let toReaderSeq getReaderState (b: Block<_, _, _>) =
            let mutable lastState: 'a option = None
            Seq.initInfinite (fun i ->
                let res = (runB b) lastState (getReaderState i)
                lastState <- Some res.state
                res)

        /// Converts a block and a given sample rate to a sequence.
        let toAudioSeq (b: Block<_, _, Env>) sampleRate =
            b
            |> toReaderSeq (fun i ->
                { samplePos = i
                  sampleRate = sampleRate })

        /// Converts a block with a sample rate of 44.1kHz to a sequence.
        let toAudioSeq44k (b: Block<_, _, _>) = toAudioSeq b 44100
        