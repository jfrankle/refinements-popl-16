module Synth.GenR
open Lang
open Util
open Skeleton
open Sequent
open Refns
open SampleJit
open System.Collections.Generic

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CACHES.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let private gen_elim_cache      = new Dictionary<sequent*int*gen_flags, seq<expr>>()
let private gen_elim_rel_cache  = new Dictionary<sequent*int*gen_flags, seq<expr>>()
let private gen_intro_cache     = new Dictionary<sequent*int*gen_flags, seq<expr>>()
let private gen_intro_rel_cache = new Dictionary<sequent*int*gen_flags, seq<expr>>()
let mutable private hits = 0.0
let mutable private attempts = 0.0

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// GENERATE ELIMINATION FORMS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let rec private gen_elim (sqnt:sequent) (depth:int) (flags:gen_flags) : seq<expr> =
    let mutable out = Seq.empty
    attempts <- attempts + 1.

    // If there's no depth left, return an empty sequence.
    if depth <= 0 then Seq.empty

    // If we've cached the result, return that instead.
    else if gen_elim_cache.TryGetValue((sqnt, depth, flags), &out) then hits <- hits + 1.; out

    // Otherwise, generate.
    else
        if sqnt.Left.IsEmpty then
            // Sample.
            if flags.jit then SampleJit.Solve(sqnt, depth) else Seq.empty
        else 
            let (skrel, sqnt') = sqnt.Peel
            let weakened = gen_elim           sqnt' depth flags
            let relevant = gen_elim_rel skrel sqnt' depth flags

            let out = Seq.append weakened relevant
            let out = out.cache()
            gen_elim_cache.[(sqnt, depth, flags)] <- out
            out

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// GENERATE ELIMINATION FORMS WITH RELEVANCE.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and private gen_elim_rel (skrel:skeleton) (sqnt:sequent) (depth:int) (flags:gen_flags) : seq<expr> =
    let mutable out = Seq.empty
    let old_sqnt = sqnt.Unpeel(skrel)
    attempts <- attempts + 1.

    // If there's no depth left, return an empty sequence.
    if depth <= 0 then Seq.empty

    // If we have precisely enough nodes to produce skrel and skrel has no holes and matches
    // up properly, then produce it.
    else if skrel.Nodes = depth && skrel.Info.StructuralCheck(sqnt.DecIn) &&
            not skrel.HasHole && skrel.GoalType = sqnt.GoalType
            && skrel.GoalRefns.SubRefines(sqnt.GoalRefns, sqnt.GoalType) then
        Seq.singleton skrel.Body

    // If this skeleton is not decreasing in decIn, return nothing.
    else if sqnt.DecIn.IsSome then Seq.empty

    // If we've cached the result, return that instead.
    else if gen_elim_rel_cache.TryGetValue((old_sqnt, depth, flags), &out) then
        hits <- hits + 1.; out

    // Otherwise, fill holes in elimination forms.
    else
        let fill_holes_rel (sk:skeleton) (depth:int) (all_may_not:bool) =
            let hs = sk.Holes.map(fun h -> (h, sk.HoleType(h)))
            let depthss = partitions depth hs.Length
            let choices = if all_may_not then [List.replicate hs.Length MayNot] :> seq<_>
                          else partitions_rel hs.Length
            let info =
                seq { for depths in depthss do
                            for choice in choices do
                                yield List.zip3 hs depths choice
                    }
            
            let gen (sks:seq<skeleton>) ((h:int, t:typ), d, ch) : seq<skeleton> =
                let decIn = if sk.FirstArg.Value = h && sk.IsRecFn then Some sk.Name else None
                // Decide how to enumerate solutions to the hole.
                let enum_with_disjunction =
                    match flags.enum_strategy with
                    | EnumRaw -> false
                    | EnumUnionBase -> sk.HoleType(h).IsBase
                    | EnumUnionEverywhere  -> true
                    | EnumUnionEverywhereUnsound -> true
                let unsound_or =
                    match flags.enum_strategy with
                    | EnumUnionEverywhereUnsound -> true
                    | _                          -> false
                let (sqnt,old_sqnt) =
                    (sqnt.SetGoalType(sk.HoleType h).SetDecIn(decIn),
                     old_sqnt.SetGoalType(sk.HoleType h).SetDecIn(decIn))
                let (sqnt, old_sqnt, skrel) =
                    if enum_with_disjunction then
                        let rs = sk.GetHole(h, unsound_or=unsound_or)
                        (sqnt.SetGoalRefns(rs), old_sqnt.SetGoalRefns(rs), skrel)
                    else
                        (sqnt.NoRefns(), old_sqnt.NoRefns(), skrel.NoRefns())

                // Do the enumeration by relevance.
                let es =
                    if decIn.IsSome then
                        match ch with
                        | May    -> gen_elim old_sqnt d flags
                        | Must   -> gen_elim_rel skrel sqnt d flags
                        | MayNot -> gen_elim sqnt d flags
                    else
                        match ch with
                        | May    -> gen_intro None old_sqnt d flags
                        | Must   -> gen_intro (Some skrel) sqnt d flags
                        | MayNot -> gen_intro None sqnt d flags
                sks |> Seq.map (fun sk -> es.map(fun e -> sk.SolveHoleCheck(h, e, sqnt.RefnGam)))
                    |> Seq.concat |> Seq.choose ident

            let fold_fn (sk:skeleton option) (h:hole) (e:expr) =
                if sk.IsSome then sk.Value.SolveHoleCheck(h, e, sqnt.RefnGam) else None
            let solve (ls:list<_>) =
                Seq.map (fun arg -> List.fold gen (Seq.singleton sk) arg) info
                |> Seq.concat |> Seq.map (fun (sk:skeleton) -> sk.Body)
            Seq.collect solve info

        let out = 
            seq {
              let candidates =
                  old_sqnt.Left.filter(fun (sk:skeleton) ->
                                         sk.HasHole &&
                                         sk.GoalType = sqnt.GoalType &&
                                         sk.StructuralCheck(sqnt.DecIn) &&
                                         depth - sk.Nodes >= sk.HoleCount)
                               .map(fun sk -> sk.ConformTo(sqnt.GoalRefns)).somes()

              // For each elimination form, including the relevant one...
              for sk in candidates do
                  // Try filling the holes.
                  yield! fill_holes_rel sk (depth-sk.Nodes) false
          
              // The case where the skeleton being applied is the relevant expression.
              if skrel.HasHole && skrel.GoalType = sqnt.GoalType &&
                 depth - skrel.Nodes >= skrel.HoleCount then
                  let skrel_conform = skrel.ConformTo(sqnt.GoalRefns)
                  if skrel_conform.IsSome then
                      yield! fill_holes_rel skrel_conform.Value (depth-skrel.Nodes) true
            }

        // Cache and return.
        let out = out.cache()
        gen_elim_rel_cache.[(old_sqnt, depth, flags)] <- out
        out

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// GENERATE INTRODUCTION FORMS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// SYNTH-SUM-R
and private synth_sum_r (skrel:skeleton option) (sqnt:sequent) (depth:int) (flags:gen_flags)
    : seq<expr> =
        // Determine which constructors we need to create.
        let (goalb, goalvs) = sqnt.GoalType.Base
        let rs' = sqnt.GoalRefns.mapv(fun r -> if r.IsBTop then r.Unfold goalvs else r)

        let ctors = 
            if sqnt.GoalRefns.IsEmpty then Ctors.AllCtors(sqnt.GoalType.Base) :> seq<_>
            else rs'.Values
                 |> List.map (fun r -> Set.ofList r.Base.Keys)
                 |> Set.intersectMany
                 |> Set.toSeq
                 |> Seq.map (fun x -> Ctors.GetCtor x goalvs)
                     
        // Create the child judgment.
        let children (c:ctor) =
            // Create the child sequent.
            let rs'        = rs'.mapv(fun r -> r.Base.[c.name]) 
            let sqnt'      = sqnt.SetGoalRefns(rs').SetGoalType(c.t)

            // Find expressions.
            let es = gen_intro skrel sqnt' (depth-1) flags
            es.map(fun e -> HC.ector(c.name, e))

        // Produce the output.
        Seq.concat (ctors.map(children))

// SYNTH-INTERSECTION-R
and synth_intersection_r (sqnt:sequent) : sequent = 
    // Break down the conjunction in each goal refinement into a list of arrows
    // without conjunction.
    let broken_ors = sqnt.GoalRefns.mapv(fun r -> r.Arr.OrList.map(HC.narr))

    // Add a new world for each conjunction-free goal refinement.
    let out = ref sqnt
    let iter_fn (w:world, r:refn) =
        let (w', seq') = (!out).DuplicateWorld(w)
        out := seq'.SetGoalRefns(seq'.GoalRefns.Add(w', r))
    List.iter iter_fn (broken_ors.Flatten())

    // Delete old worlds.
    (!out).DeleteWorlds(broken_ors.Keys)

// SYNTH-UNION-R
and synth_union_r (skrel:skeleton option) (sqnt:sequent) (depth:int) (flags:gen_flags)
    : seq<expr> =
    // Create the refns for each subproblem.
    let rfs = sqnt.GoalRefns.BreakUnions().Cartesian()

    // Create each subproblem.
    rfs.map(fun rf -> gen_intro skrel (sqnt.SetGoalRefns(rf)) depth flags) |> Seq.concat

// SYNTH-IMPL-R
and synth_impl_r (skrel:skeleton option) (sqnt:sequent) (depth:int) (flags:gen_flags)
    : seq<expr> =
    // If this has intersection, call SYNTH-INTERSECTION-R and continue.
    let sqnt = if skrel.IsSome then sqnt.Unpeel(skrel.Value) else sqnt
    let sqnt = if sqnt.GoalRefns.HasIntersection() then synth_intersection_r(sqnt) else sqnt
    let (skrel, sqnt) = if skrel.IsSome then let (a, b) = sqnt.Peel in (Some a, b)
                        else (None, sqnt)

    // If this has union, call SYNTH-UNION-R and stop.
    if sqnt.GoalRefns.HasUnion() then synth_union_r skrel sqnt depth flags
    else
        // All conjunction and disjunction is gone by this point.
        let (t1, t2) = sqnt.GoalType.Arr

        // Create identifiers for the argument and recursive function.
        let x    = (sqnt.Unfocused.map(LangExtensions.ToInfoT) @ Library.Info()).ToGam().Fresh(t1)

        // Create a refinement for the recursive function and a sequent for the subproblem.
        let (rf1, rf2) = sqnt.GoalRefns.SplitArrow()
        let x_info     = {name=x; t=t1; rs=rf1; about=AIUninteresting}
        let sqnt'      = sqnt.SetGoalType(t2).SetGoalRefns(rf2)
        let (sqnt', skrel') = 
            if skrel.IsSome then
                let skrel_ref = ref skrel.Value
                (sqnt'.AddLeft([x_info], also=skrel_ref), Some skrel_ref.Value)
            else
                (sqnt'.AddLeft([x_info]), None)

        // Create the synthesis subproblem and rule.  We can't be at the top level anymore.
        let es = gen_intro skrel' sqnt' (depth-1) flags
        es.map(fun e -> HC.efun{argl=x_info.name; arg_type=t1; body=e})

// SYNTH-PRODUCT-R.
and synth_product_r (skrel:skeleton option) (sqnt:sequent) (depth:int) (flags:gen_flags)
    :seq<expr> =
    let ts = sqnt.GoalType.Tup

    // If this has union, call SYNTH-UNION-R and stop.
    if sqnt.GoalRefns.HasUnion() then synth_union_r skrel sqnt depth flags
    elif depth - 1 < ts.Length then Seq.empty
    else
        // Extract the singleton tuple in each refn.
        let rfs = sqnt.GoalRefns.SplitTuple(ts.Length)

        // Create the subproblems.
        let sqnts = List.map2 (fun t rs -> sqnt.SetGoalType(t).SetGoalRefns(rs)) ts rfs
        let depthss = partitions (depth-1) ts.Length |> Seq.map (List.zip sqnts)

        if skrel.IsNone then
            let create_tups ls =
                Seq2.cartesianList (List.map (fun (sqnt, d) -> gen_intro None sqnt d flags) ls)
                |> Seq.map HC.etup
            Seq.collect create_tups depthss

        else
            let choices = partitions_rel ts.Length
            let info =
              seq { for depths in depthss do
                        for choice in choices do
                            yield List.zip depths choice }
            let create_tups info =
                let gen ((sqnt:sequent, d:int), ch:choice) =
                    match ch with
                    | MayNot -> gen_intro None  sqnt d flags
                    | Must   -> gen_intro skrel sqnt d flags
                    | May    -> gen_intro None  (sqnt.Unpeel(skrel.Value)) d flags
                Seq2.cartesianList (List.map gen info)
                |> Seq.map HC.etup
            Seq.collect create_tups info

and private gen_intro (skrel:skeleton option) (sqnt:sequent) (depth:int) (flags:gen_flags)
    : seq<expr> =
    // Main body of function.
    let mutable out = Seq.empty
    let cache = if skrel.IsSome then gen_intro_rel_cache else gen_intro_cache
    attempts <- attempts + 1.
    let key = if skrel.IsSome then sqnt.Unpeel(skrel.Value) else sqnt

    // If there's no depth left, return an empty sequence.
    if depth <= 0 then Seq.empty

    // If we've cached the result, return that instead.
    else if cache.TryGetValue((key, depth, flags), &out) then
        hits <- hits + 1.; out

    // Otherwise, generate.
    else    
      let out =
        if sqnt.Left.IsEmpty || skrel.IsSome then
                match sqnt.GoalType.Node with
                // NO MATCH STATEMENT - NOT COMPLETE.
                | TUnit        -> if depth = 1 && skrel.IsNone && sqnt.Left.IsEmpty
                                  then Seq.singleton HC.eunit
                                  else Seq.empty
                | TBase(b, vs) -> synth_sum_r     skrel sqnt depth flags
                | TArr(t1, t2) -> synth_impl_r    skrel sqnt depth flags
                | TTup ts      -> synth_product_r skrel sqnt depth flags
                | TPoly _      -> Seq.empty

        else 
            let (skrel, sqnt') = sqnt.Peel
            let weakened = gen_intro         None sqnt' depth flags
            let relevant = gen_intro (Some skrel) sqnt' depth flags
            weakened.append(relevant)
      let out = if skrel.IsSome then out.append(gen_elim_rel skrel.Value sqnt depth flags) else out
      let out = out.cache()
      cache.[(key, depth, flags)] <- out
      out

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// GENERATE EXPRESSIONS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Generate all ASTs obeying sequent sqnt of exactly size depth.
let public GenR(sqnt:sequent, depth:int, flags:gen_flags) : seq<expr> =
    if sqnt.DecIn.IsNone then
        gen_intro None sqnt depth flags
    else
        gen_elim sqnt depth flags

// Generate all ASTs at type t of exactly size depth.
let public GenT(ids:id_info_t list, t:typ, depth:int, decIn:id option) : seq<expr> =
    let sqnt = sequent.Create(t).AddLeft(ids.map(fun info -> info.ToInfo(Map.empty)))
    GenR(sqnt, depth, {jit=false; enum_strategy=EnumRaw})

let public Ratio() = hits / attempts

