module Synth.SampleAot
open Util
open Lang
open Eval
open System.Collections.Generic

let GenFn : (id_info_t list * typ * int * id option -> seq<expr>) ref = ref (make Seq.empty)

// The ahead-of-time sampling cache.
let private cache = new Dictionary<value * int, refn option * bool>()

// The boolean represents whether we used up all of our depth on some branch.
// This ensures that we return only values with the depth specified.
// n is the depth.
let rec private sample_aot(v, t:typ, n:int) : refn option * bool =
    // Base case.
    if n <= 0 then (None, false)

    // Check for a cache hit.
    else if cache.ContainsKey(v, n) then cache.[(v, n)]

    // Otherwise, recur.
    else
        let result =
            match t.Node, v.Node with
            | TUnit,   VUnit        -> (Some HC.nutop, n=1)
            | TBase(b, vs), VCtor(c, v') ->
                let t' = Ctors.Type c vs
                let (r', b) = sample_aot(v', t', n-1)
                if r'.IsNone then (None, false) else (Some (HC.nbase(Map.empty.Add(c, r'.Value))), b)
            | TTup ts, VTup vs      -> 
                let (news, bs) = List.unzip (List.map2 (fun t v -> sample_aot(v, t, n-1)) ts vs)
                let b = List.exists ident bs
                if List.exists Option.isNone news then (None, false)
                else (Some (HC.ntup[news.map(Option.get)]), b)
            | TArr(t1, t2), VFun f  ->
                // ***** Be warned - this part is complicated *****
                 // Infrastructure for evaluation.
                let eval_env = if f.self.IsSome then f.env.Add(f.self.Value, v) else f.env
                let eval_gam = if f.self.IsSome then f.gam.Add(f.self.Value, t) else f.gam
                let eval arg = f.body.Eval(eval_gam.Add(f.argv, t1)) (eval_env.Add(f.argv, arg)) t2

                // ***** Theory time *****
                //
                // Assume, by induction, that we have already created:
                // [refinements <= size n-2 for arguments <= size n-2 and
                //  refinements <= size n-2 for outputs of evaluation]
                //
                // To preserve this invariant, we need to create three new groups of refinements:
                // 1) [refinements <= size n-2 for arguments <= size n-2 and
                //     refinements  = size n-1 for outputs of evaluation]
                // 2) [refinements  = size n-1 for arguments <= size n-2 and
                //     refinements <= size n-1 for outputs of evaluation]
                // 3) [refineemnts <= size n-1 for arguments  = size n-1 and
                //     refinements <= size n-1 for outputs of evaluation]

                // Get all arguments <= size n-2 and = size n-1.
                let gam = Library.Gam()
                let info = Library.Info()
                let args_lt  = Seq.collect (fun n -> GenFn.Value(info, t1, n, None)) (seq{1..n-2})
                let args_eq  = GenFn.Value(info, t1, n-1, None)

                // Evaluate the arguments.
                let env = Library.Env()
                let args_lt = Seq.map (fun (e:expr) -> e.Eval gam env t1) args_lt
                let args_eq = Seq.map (fun (e:expr) -> e.Eval gam env t1) args_eq

                // Evaluate.
                let evals_lt = args_lt.map(fun e -> (e, eval e))
                let evals_eq = args_eq.map(fun e -> (e, eval e))

                // 1) [refinements <= size n-2 for arguments <= size n-2 and
                //     refinements  = size n-1 for outputs of evaluation]
                let part1 = evals_lt.map(fun (i, o) -> (sample_aot_to i t1 (n-2), sample_aot(o, t2, n-1)))
                                .filter(fun ((i,bi),(o,bo)) -> i.IsSome && o.IsSome && bo)
                                .map(   fun ((i,bi),(o,bo)) -> (i.Value, o.Value))

                // 2) [refinements  = size n-1 for arguments <= size n-2 and
                //     refinements <= size n-1 for outputs of evaluation]
                let part2 = evals_lt.map(fun (i, o) -> (sample_aot(i, t1, n-1), sample_aot_to o t2 (n-1)))
                                .filter(fun ((i,bi),(o,bo)) -> i.IsSome && o.IsSome && bi)
                                .map(   fun ((i,bi),(o,bo)) -> (i.Value, o.Value))

                // 3) [refinements <= size n-1 for arguments  = size n-1 and
                //     refinements <= size n-1 for outputs of evaluation]
                let part3 = evals_eq.map(fun (i, o) -> (sample_aot_to i t1 (n-1), sample_aot_to o t2 (n-1)))
                                .filter(fun ((i,bi),(o,bo)) -> i.IsSome && o.IsSome)
                                .map(   fun ((i,bi),(o,bo)) -> (i.Value, o.Value))

                // Produce the eventual refinement.
                let cnfs = part1.append(part2).append(part3)
                                .map(fun (a, b) -> [(a, b)])
                                .cache()
                if cnfs.IsEmpty then (None, false)
                else (Some (HC.narr(CNF.Create(Seq.toList cnfs))), true)
            | _, _ -> failwith "Type-value mismatch."

        // Cache and return.
        cache.[(v, n)] <- result
        result

// Infrastructure for ahead-of-time sampling.
and private sample_aot_to v t n : refn option * bool =
    // "None" does not mean uninhabited - it just means that nothing
    // was found at a particular size.  We only intersect refinements
    // from those sizes where something was found.
    let (rs, bs) = List.unzip ([1..n].map(fun n -> sample_aot(v, t, n)))
    let rs = rs.filter(Option.isSome).map(Option.get)
    let b  = List.exists ((=) true) bs
    if rs.IsEmpty then (None, false)
    else
        let r = refn.PushAnd(rs, t)
        (Some r, b)

// External facing ahead-of-time sampling at a particular depth.
let public SampleAot(v:value, t:typ, depth:int) : refn option =
    let f() = sample_aot(v, t, depth) |> ignore
    runWithStack f 10000000

    // Should get a cache hit.  Keeps us from having to pass data between threads.
    let (result, b) = sample_aot(v, t, depth)
    if b then result else None

// External facing ahead-of-time sampling to a particular depth.
let public SampleAotTo(v:value, t:typ, depth:int) : refn option =
    let f() = sample_aot_to v t depth |> ignore
    runWithStack f 10000000

    // Should get a cache hit.  Keeps us from having to pass data between threads.
    let (result, b) = sample_aot_to v t depth
    if b then result else None