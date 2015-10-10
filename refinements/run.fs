module Synth.Run
open System
open Util
open Lang
open Normalize
open Judgment
open Eval
open System.Threading.Tasks

type RunMode = RSynthesize | REnumerate of int | RSample of string * int
type SampleMode = SJit | SAot of int
type Timing = TimeNo | TimeYes | TimeData

// Read an input file.
let private ReadFile(filename : string) : string =
    (new IO.StreamReader(filename)).ReadToEnd()

// Parse a string into a synthesis problem.
let private Parse(input : string) : Choice<synth_problem, string> =
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString input
    try Choice1Of2(Parser.start Lexer.token lexbuf) with
    | e -> Choice2Of2(e.Message)

// Typecheck a synthesis problem.
let private Typecheck(prob : synth_problem) : Choice<refn_ext option, string> =
    Typecheck.Run(prob, expr.EvalFn, Library.Add)

// Normalize the goal refinement into its internal form.
let private Normalize(r : refn_ext, t : typ, sampling:SampleMode) : Choice<refn, string> =
    let sample(v, t) = 
        match sampling with
        | SJit -> None
        | SAot n -> SampleAot.SampleAotTo(v, t, n)
    try Choice1Of2(r.NegElim(t).Normalize(t, sample).Simplify(t)) with
    | e -> Choice2Of2(e.Message)

let private CheckInhabited(r:refn, t:typ) : bool =
    r.Inhabited(t)

// Create an initial sequent and idctx for synthesis.
let private CreateSequent(r:refn, t:typ, is_rec:id option, sampling:SampleMode)
    : Sequent.sequent  =

    // Preload the library.
    let seq     = Sequent.sequent.Create(t, r)

    // Ahead-of-time sampling if demanded.
    let seq =
       match sampling with
       | SJit    -> seq
       | SAot(n) ->
           let map_fn (x, t, r:refn option) : id_info =
               {name  = x
                t     = t
                rs    = seq.GoalRefns.mapv(make r.Value)
                about = AIUninteresting }

           let refns = Library.Gam()
                              .map(fun x t -> x, t, SampleAot.SampleAotTo(Library.Env().[x], t, n))
                              .Values
                              .filter(fun (a, b, c) -> c.IsSome)
                              .map(map_fn)
           seq.AddLeft(refns)
            
    // Return the result.
    seq

// Run synthesis.
let private SynthesizeEnumerate(r:refn, t:typ, is_rec:id option, n:int,
                                sampling:SampleMode, enum_strategy:enum_strategy,
                                verbose:bool)
    : list<expr> =
    let seq = CreateSequent(r, t, is_rec, sampling)
    let jit = sampling = SJit
    SynthDriver.Enumerate(seq, n, is_rec, enum_strategy, jit, verbose)

// Run synthesis.
let private Synthesize(name:string, r:refn, t:typ, is_rec:id option,
                       sampling:SampleMode, enum_strategy:enum_strategy,
                       verbose:bool, print_time:Timing, refn_sz:int)
    : expr option =
    let seq = CreateSequent(r, t, is_rec, sampling)
    let jit = sampling=SJit

    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let run = async { return SynthDriver.Synthesize(seq, is_rec, enum_strategy, jit, verbose) }
    let soln = Async.RunSynchronously(Async.AwaitTimeout(run, 100000))
    //let soln = Some (SynthDriver.Synthesize(seq, is_rec, enum_strategy, jit, verbose))
    stopwatch.Stop()

    if print_time = Timing.TimeYes then
        printfn "Running time: %d ms" (stopwatch.ElapsedMilliseconds)
    elif print_time = Timing.TimeData then
        if soln.IsNone then printfn "%s,%d,%s,%f" name refn_sz "timeout" (GenR.Ratio())
        else                printfn "%s,%d,%d,%f" name refn_sz stopwatch.ElapsedMilliseconds (GenR.Ratio())

    soln

// Run refcon.
let private Sample(name:id, n:int) =
    if not(Library.Gam().ContainsKey(name)) then printfn "Binding %s not found." name
    if n <= 0                               then printfn "Invalid number of steps: %d." n

    let t = Library.Gam().[name]
    for i in [1..n] do
        let ro = SampleAot.SampleAot(Library.Env().[name], t, i)
        if ro.IsSome then printfn "At size %d:\n%O\n" i ro.Value

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// MAIN DRIVER METHOD.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// The main method.
let public Run(inputfile:string, outputfile:string option,
               mode:RunMode, verbose:bool, sampling:SampleMode,
               enum_strategy:enum_strategy, inhabit_check:bool, print_time:Timing) =

    // Create a stream for the output.
    let outstream   =
        match outputfile with
        | Some s -> new IO.StreamWriter(s)
        | _      -> new IO.StreamWriter(Console.OpenStandardOutput())
    outstream.AutoFlush <- true
    let input = ReadFile(inputfile)

    // Parse.
    match Parse(input) with
    | Choice2Of2 errmsg -> outstream.WriteLine("Parsing failed:\n" + errmsg)
    | Choice1Of2 sp     ->

      // Typecheck.
      match Typecheck(sp) with
      | Choice2Of2 errmsg -> outstream.WriteLine("Typechecking failed:\n" + errmsg)
      | Choice1Of2 r_ext  ->

        // Normalize.
        let r_int =
          match r_ext with
          | None -> Choice1Of2(sp.synth_type.Top)
          | Some r_ext -> Normalize(r_ext, sp.synth_type, sampling)

        match r_int with
        | Choice2Of2 errMsg -> outstream.WriteLine("Normalization failed:\n" + errMsg)
        | Choice1Of2 r_int ->

            // Check for inhabitants.
            if inhabit_check && not(CheckInhabited(r_int, sp.synth_type)) then
                outstream.WriteLine("Synthesis problem is impossible: no solution exists.")
            else
              // Choose what to do.
              let rec_name = if sp.is_rec then Some sp.synth_name else None

              match mode with
              | RSample(name, depth) -> Sample(name, depth)
              | RSynthesize ->
                  let nodes = if sp.synth_refn.IsNone then 0 else sp.synth_refn.Value.Nodes
                  let out = Synthesize(sp.synth_name, r_int, sp.synth_type, rec_name, sampling,
                                       enum_strategy, verbose, print_time, nodes)
                  let outstr = if out.IsSome then out.Value.ToString() else "timeout"
                  if print_time <> Timing.TimeData then
                      outstream.WriteLine(sprintf "%s" outstr)
              | REnumerate(depth) ->
                    for n in [1..depth] do
                        outstream.WriteLine(sprintf "Enumerating at depth %d:" n)
                        let out = SynthesizeEnumerate(r_int, sp.synth_type, rec_name, n, sampling,
                                                      enum_strategy, verbose)
                        Seq.iter (fun (e:expr) -> outstream.WriteLine(sprintf "%O" e)) out
