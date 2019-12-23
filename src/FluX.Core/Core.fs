
module FluX.Core

[<Struct>]
type Res<'a, 'b> =
    { value: 'a
      state: 'b }

type Block<'v, 's, 'r> = Block of ('s option -> 'r -> Res<'v, 's>)

let runBlock m =
    match m with
    | Block x -> x

[<Struct>]
type Pair<'a, 'b> =
    { mine: 'a
      other: 'b }

let bind (m: Block<'a, 'sa, 'r>) (f: 'a -> Block<'b, 'sb, 'r>): Block<'b, Pair<'sa, 'sb>, 'r> =
    let stateFunc localState readerState =
        let initialMatch =
            match localState with
            | None ->
                { mine = None
                  other = None }
            | Some v ->
                { mine = Some v.mine
                  other = Some v.other }

        let prevAState = initialMatch.mine
        let prevBState = initialMatch.other
        let a = (runBlock m) prevAState readerState
        let fRes = f a.value
        let b = (runBlock fRes) prevBState readerState

        { value = b.value
          state =
              { mine = a.state
                other = b.state } }

    Block stateFunc

let ret x =
    Block(fun _ _ ->
        { value = x
          state = () })

let (!) = ret

let retFrom l = l

type LoopBaseBuilder() =
    member __.Bind(m, f) = bind m f
    member __.Return x = ret x
    member __.ReturnFrom l = retFrom l

let loopBase = LoopBaseBuilder()

type LoopGenBuilder<'a>() =
    member __.Bind(m: Block<_, _, 'a>, f) = bind m f
    member __.Return x = ret x
    member __.ReturnFrom l = retFrom l

let loopGen<'a> = LoopGenBuilder<'a>()

/// TODO: Docu
let map l f =
    let f1 =
        fun p r ->
            let res = runBlock l p r
            let mappedRes = f res.value
            { value = mappedRes
              state = res.state }
    Block f1
/// map operator
let (<!>) = map

/// TODO: Docu (applicative)
let (<*>) (f: Block<'f, _, 'r>) (l: Block<'v1, _, 'r>): Block<'v2, _, 'r> =
    loopBase {
        let! resL = l
        let! innerF = f
        let result = innerF resL
        return result
    }

let (<**>) (f: Block<'f, _, 'r>) (l: Block<'v1, _, 'r>): Block<'v2, _, 'r> =
    loopBase {
        let! resL = l
        let! innerF = f
        let result = innerF resL
        return! result
    }

// TODO: Use this
(*
type ArithmeticExt = ArithmeticExt with
static member inline (?<-) (ArithmeticExt, a: Block<'v,'s>, b) =
    block {
        let! aValue = a
        return aValue + b
    }
static member inline (?<-) (ArithmeticExt, a, b: Block<'v,'s>) =
    block {
        let! bValue = b
        return a + bValue
    }
static member inline (?<-) (ArithmeticExt, a: Block<'v1,'s1>, b: Block<'v2,'s2>) =
    block {
        let! aValue = a
        let! bValue = b
        return aValue + bValue
    }
static member inline (?<-) (ArithmeticExt, a, b) = a + b

let inline (+) a b = (?<-) ArithmeticExt a b
*)

let inline private binOpBoth left right f =
    loopBase {
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
    loopBase {
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
    loopBase {
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
    let read() = Block(fun p r -> { value=r; state=()} )

    /// Lifts a function with an initial value.
    let liftSeed seed l =
        fun p r ->
            let x = match p with
                    | Some previousState -> previousState
                    | None -> seed
            l x r


[<AutoOpen>]
module Feedback =

    [<Struct>]
    type Fbd<'a, 'b> =
        { feedback: 'a
          out: 'b }

    /// Feedback with reader state
    let (<=>) seed (f: 'a -> 'r -> Block<Fbd<'a, 'v>, 's, 'r>) =
        let f1 =
            fun prev r ->
                let myPrev, innerPrev =
                    match prev with
                    | None -> seed, None
                    | Some(my, inner) -> my, inner

                let lRes = runBlock (f myPrev r) innerPrev r
                let feed = lRes.value
                let innerState = lRes.state
                { value = feed.out
                  state = feed.feedback, Some innerState }
        Block f1
