module Synth.Judgment
open Util
open Lang
open Refns
open Sequent
open System
open Skeleton

// Patched in within the functions of SynthDriver.fs
let public EnumerateFn : (sequent * int * id option * enum_strategy * bool * bool -> expr list) ref =
    ref (fun _ -> failwith "Enumerate function not patched.")

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// JUDGMENT PRELIMINARIES.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// The type of judgment.
type private judgment_type =
    | JUnit
    | JSolved of expr
    | JFunc   of judgment * id * id option
    | JTuple  of judgment list * list<seq<expr>>
    | JBase   of base_judgment_type
    | JOr     of judgment list
    | JCtor   of judgment * id
    | JUndetermined
    | JImpossible

    // Properties.
    member public this.Undetermined with get() = match this with JUndetermined -> true | _ -> false
    member public this.Impossible   with get() = match this with JImpossible   -> true | _ -> false

// The description of a base judgment.
and base_subproblem = skeleton * hole * int * id option
and private base_judgment_type = {
    ctors     : list<judgment>
    off       : list<skeleton>
    m_off     : list<skeleton>
    on        : list<base_subproblem>
    m_on      : list<base_subproblem>
    solved    : list<expr>
    matches   : list<match_entry>
    last_scrut_size : int
}

// The description of a match entry.
and private match_entry = {
    scrut    : expr
    patterns : (id * id) list
    branches : judgment list
    solved   : seq<expr> list
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// A SYNTHESIS JUDGMENT.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and private judgment_rep = {
    mutable enum      : bool          // Whether we are in enumerate mode or solve mode.
    mutable seq       : sequent       // The sequent on the bottom of this judgment.
    mutable depth     : int           // The maximum depth of this expression.
    mutable rule      : judgment_type // The type of judgment.
    mutable matches   : int           // The number of matches allowed.
    mutable top_level : bool          // Whether we are at the top-level function of the term.
    is_rec            : id option     // The name of the function if it is recursive.
    mutable scrut_size: int           // The maximum scrutinee size allowed.
    jit               : bool          // Whether to do just-in-time sampling of libraries.
    enum_strategy     : enum_strategy // The enumeration strategy to use.
    mutable no_matches: bool          // If true, don't synthesize any matches.
}

// A synthesis judgment.
and public judgment private(rep : judgment_rep) =
    member private this.Rep = rep

    // Create the top-level synthesis step.
    static member public Create(initial_seq:sequent, enum_strategy:enum_strategy,
                                ?jit:bool, ?is_rec:id option, ?enum:bool, ?no_matches:bool) =
        judgment({
                    enum          = defaultArg enum false
                    seq           = initial_seq
                    depth         = 0
                    rule          = JUndetermined
                    is_rec        = defaultArg is_rec None
                    matches       = 0
                    top_level     = true
                    scrut_size    = 6
                    jit           = defaultArg jit false
                    enum_strategy = enum_strategy
                    no_matches    = defaultArg no_matches false
                 }) 

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // PROPERTIES.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Properties.
    member private this.Impossible  with get() = this.Rep.rule.Impossible
    member private this.GoalRefns   with get() = this.Rep.seq.GoalRefns
    member private this.GoalType    with get() = this.Rep.seq.GoalType

    // Function variants.
    static member private ImpossibleFn(j:judgment) = j.Impossible

    // Private properties.
    member private this.DoneRefining with get()  = not(this.Rep.rule.Undetermined)
    member private this.Seq          with get() = this.Rep.seq and set(v) = this.Rep.seq <- v

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // MUTATORS.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    member private this.MarkImpossible() = this.Rep.rule <- JImpossible
    member private this.Child(?seq, ?depth, ?rule, ?matches, ?top_level, ?enum,
                              ?scrut_size, ?elim_size, ?no_matches) =
        judgment({
                   enum          = defaultArg enum       this.Rep.enum
                   seq           = defaultArg seq        this.Rep.seq
                   depth         = defaultArg depth      this.Rep.depth
                   matches       = defaultArg matches    this.Rep.matches
                   rule          = defaultArg rule       JUndetermined
                   top_level     = defaultArg top_level  this.Rep.top_level
                   is_rec        = this.Rep.is_rec
                   scrut_size    = defaultArg scrut_size this.Rep.scrut_size
                   jit           = this.Rep.jit
                   enum_strategy = this.Rep.enum_strategy
                   no_matches    = defaultArg no_matches this.Rep.no_matches
                 })
    member private this.Copy() = this.Child(rule=this.Rep.rule)

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // RIGHT RULES.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // SYNTH-INTERSECTION-R.  Handle intersection on the right.
    member private this.SynthIntersectionR() = 
        // Only arrow refinements can contain intersection.
        if not(Map.forall (fun _ (r:refn) -> r.IsArr) this.GoalRefns) then
            failwith "Intersection can only appear in arrows."

        // Break down the conjunction in each goal refinement into a list of arrows
        // without conjunction.
        let broken_ors = this.GoalRefns.mapv(fun r -> r.Arr.OrList.map(HC.narr))

        // Add a new world for each conjunction-free goal refinement.
        let iter_fn (w:world, r:refn) =
            let (w', seq') = this.Seq.DuplicateWorld(w)
            this.Seq <- seq'.SetGoalRefns(seq'.GoalRefns.Add(w', r))
        List.iter iter_fn (broken_ors.Flatten())

        // Delete old worlds.
        this.Seq <- this.Seq.DeleteWorlds(broken_ors.Keys)

    // SYNTH-UNION-R.  Handle union on the right.
    member private this.SynthUnionR() =
        // We only break unions in arrow and tuple refinements.
        if not(Map.forall (fun _ (r:refn) -> r.IsArr || r.IsTup) this.GoalRefns) then
            failwith "Union can only be broken in arrow and tuple refinements."

        // Create the refns for each subproblem.
        let rfs = this.GoalRefns.BreakUnions().Cartesian()

        // Create each subproblem.
        let create_subproblem (rf:refns) = this.Child(seq=this.Seq.SetGoalRefns(rf))
        this.Rep.rule <- JOr(rfs.map(create_subproblem))
        
    // SYNTH-UNIT-R.  Produces the unit value.
    member private this.SynthUnitR() =
        if Map.exists (fun _ (r:refn) -> r.IsUBot) this.GoalRefns then
            this.MarkImpossible()
        else this.Rep.rule <- JUnit

    // SYNTH-IMPL-R.  Creates a recursive function and subproblem for the body.
    member private this.SynthImplR() =
        // Add the recursive function to each world if we are recursive.
        let f = (this.Seq.Unfocused.map(LangExtensions.ToInfoT) @ Library.Info()).ToGam()
                    .Fresh(this.Seq.GoalType)
        if this.Rep.is_rec.IsSome then
            let f_rs = this.Seq.GoalRefns
            let f_info = {name=f; t=this.Seq.GoalType; rs=f_rs; about=AIRecFn}
            this.Seq <- this.Seq.AddLeft([f_info])

        // If this has intersection, call SYNTH-INTERSECTION-R and continue.
        if this.GoalRefns.HasIntersection() then this.SynthIntersectionR()

        // If this has union, call SYNTH-UNION-R and stop.
        if this.GoalRefns.HasUnion() then this.SynthUnionR()
        else
            // All conjunction and disjunction is gone by this point.
            let (t1, t2) = this.GoalType.Arr

            // Create identifiers for the argument and recursive function.
            let gam = (this.Seq.Unfocused.map(LangExtensions.ToInfoT) @ Library.Info()).ToGam()
            let x   = gam.Add(f, this.Seq.GoalType).Fresh(t1)

            // Create a refinement for the recursive function and a sequent for the subproblem.
            let (rf1, rf2) = this.GoalRefns.SplitArrow()
            let about      = if this.Rep.is_rec.IsNone || not this.Rep.top_level then AIUninteresting
                             else AIArgOf f
            let x_info     = {name=x; t=t1; rs=rf1; about=about}
            let seq'       = this.Rep.seq.SetGoalType(t2)
                                         .SetGoalRefns(rf2)
                                         .AddLeft([x_info])

            // Create the synthesis subproblem and rule.  We can't be at the top level anymore.
            let subproblem = this.Child(seq=seq', depth=this.Rep.depth - 1)

            this.Rep.rule <- JFunc (subproblem, x, if this.Rep.is_rec.IsSome then Some f else None)

    // SYNTH-PRODUCT-R.  Creates a tuple and subproblems for each constituent.
    member private this.SynthProductR() =
        // If this has union, call SYNTH-UNION-R and stop.
        if this.GoalRefns.HasUnion() then this.SynthUnionR()
        else
            // Extract the singleton tuple in each refn.
            let ts = this.GoalType.Tup

            // Create a subproblem for a particular index of a tuple.
            let create_tuple_subproblem (t:typ) (rf:refns) =
                this.Child(seq   = this.Seq.SetGoalType(t).SetGoalRefns(rf),
                           depth = this.Rep.depth - 1) 

            // Create the main subproblem.
            let rfs    = this.GoalRefns.SplitTuple(ts.Length)
            let js     = List.map2 create_tuple_subproblem ts rfs

            this.Rep.rule <- JTuple(js, List.replicate ts.Length Seq.empty)

    // SYNTH-SUM-R.
    // Synthesizes child judgments that use constructors.
    member private this.SynthSumR() : judgment list =
        // Determine which constructors we need to create.
        let (goalb, goalvs) = this.GoalType.Base
        let rs' = this.GoalRefns.mapv(fun r -> if r.IsBTop then r.Unfold goalvs else r)

        let ctors = 
            if this.GoalRefns.IsEmpty then Ctors.AllCtors(this.GoalType.Base)
            else rs'.Values
                 |> List.map (fun r -> Set.ofList r.Base.Keys)
                 |> Set.intersectMany
                 |> Set.toList
                 |> List.map (fun x -> Ctors.GetCtor x goalvs)
                     
        // Create the child judgment.
        let make_judgment (c:ctor) =
            // Create the child judgment.
            let rs'        = rs'.mapv(fun r -> r.Base.[c.name]) 
            let seq'       = this.Rep.seq.SetGoalRefns(rs').SetGoalType(c.t)
            let subproblem = this.Child(seq = seq', depth = this.Rep.depth - 1, no_matches=true)

            this.Child(rule = JCtor(subproblem, c.name))

        // Produce the output.
        List.map make_judgment ctors

    // SYNTH-SUM-L.  Produces a match statement given scrutinee information.
    // Assumes that the skeleton is solved (has no holes).
    member private this.SynthSumL(scrut:expr, scrut_info:id_info, scrut_t:typ, scrut_rs:refns)
        : match_entry option =
        // Make sure our scrutinee covers all match branches.
        if not(scrut_rs.ContainsAllCtors(scrut_t)) then None
        else
            // Create a synthesis problem for a branch.
            let create_branch (c : ctor) =
                // Find worlds that this constructor.  Purge all others.
                let rs' = scrut_rs.mapv(fun r -> if r.IsBTop then r.Unfold (snd scrut_t.Base) else r)
                let matches_ctor _ (r:ctor_map) = r.ContainsKey(c.name)
                let (yes, no) = Map.partition matches_ctor (rs'.mapv(refn.BaseFn))
                let seq = this.Seq.DeleteWorlds(no.Keys)

                // Create a new synthesis problem.
                let x    = (this.Seq.Unfocused.map(LangExtensions.ToInfoT) @ Library.Info()).ToGam().Fresh(c.t)
                let about   = if scrut_info.about.ArgOf.IsSome then AIDecIn scrut_info.about.ArgOf.Value
                              else AIUninteresting
                let x_info = {name=x; t=c.t; rs=yes.mapv(fun (m:ctor_map) -> m.[c.name]);
                              about=about}
                let seq    = seq.AddLeft([x_info])
                                .RemoveLeft(scrut_info.name)
                (c.name, x, this.Child(seq = seq, depth = 0, matches = this.Rep.matches - 1))

            let branches = Ctors.AllCtors(scrut_t.Base).map(create_branch)
                                .map(fun (c, x, j) -> ((c, x), j))
                            |> List.unzip
            Some {scrut = scrut; patterns = fst branches; branches = snd branches;
                  solved = (fst branches).map(make Seq.empty) }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // REFINEMENT.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Set the synthesis rule properly for base type.
    member private this.SetBasePolyRule() : unit =
        // Only create constructors if we aren't decreasing.
        let ctors = if this.GoalType.IsBase && this.Seq.DecIn.IsNone then this.SynthSumR() else []

        // Expressions to be pulled from the context and used to solve the goal (SYNTH-CTX).
        // Ensure that all expressions are of the proper type and fit the goal refinements.
        // Ensure that we only pick expressions that are properly decreasing.
        let off = this.Seq.Left
                    .filter(fun s -> s.GoalType = this.GoalType)
                    .map(fun s -> s.ConformTo(this.GoalRefns)).somes()
                    .filter(fun s -> s.StructuralCheck(this.Seq.DecIn))

        // Expressions to be pulled from the context and used as scrutinees in match statements
        // (SYNTH-SUM-L).  Ensure that their goals contain all constructors and they are properly
        // decreasing.
        let m_off =
            if this.Rep.no_matches || not this.GoalType.IsBase then []
            else    this.Seq.Left
                      .filter(fun s -> s.GoalType.IsBase)
                      .filter(fun s -> s.GoalContainsAllCtors())
                      .filter(fun s -> s.StructuralCheck(this.Seq.DecIn))

        // Create the rule.
        // No expressions are currently in consideration or solved, so those values are empty lists.
        this.Rep.rule <- JBase {ctors=ctors; off=off; m_off=m_off;
                                on=[]; m_on=[]; solved=[]; matches=[];
                                last_scrut_size=0}

    member private this.Refine() =
        // Refine in a type-directed manner.  Everything requires depth.
        if not(this.DoneRefining) && this.Rep.depth > 0 then
            match this.GoalType.Node with
            | TUnit   -> this.Rep.top_level <- false; this.SynthUnitR()
            | TArr  _ -> this.SynthImplR()
            | TTup  _ -> this.Rep.top_level <- false; this.SynthProductR()
            | TBase _ ->
                this.Rep.top_level <- false
                this.SetBasePolyRule()
            | TPoly _ ->
                this.Rep.top_level <- false
                this.Rep.no_matches <- true
                this.SetBasePolyRule()
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // METHODS TO ADVANCE THE SYNTHESIS PROCESS.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Increase the permissible expression depth {d}, match depth {m}, and scrutinee depth {s}.
    member public this.AddDepth(d, m, s) =
        this.Rep.depth      <- this.Rep.depth      + d
        this.Rep.matches    <- this.Rep.matches    + m
        this.Rep.scrut_size <- this.Rep.scrut_size + s

        match this.Rep.rule with
        | JUndetermined | JImpossible | JSolved _ | JUnit -> ()
        | JCtor(j, _)      -> j.AddDepth(d, m, s)
        | JFunc(j, _, _)   -> j.AddDepth(d, m, s)
        | JTuple(js, _)    -> js.iter(fun j -> j.AddDepth(d, m, s))
        | JOr js           -> js.iter(fun j -> j.AddDepth(d, m, s))
        | JBase b          ->
            b.ctors.iter(fun j  -> j.AddDepth(d, m, s))
            b.matches.iter(fun mtch -> mtch.branches.iter(fun (j:judgment) -> j.AddDepth(d, m, s)))

    // Take a synthesis step at base type.  Uses a number of heuristics and strategies
    // to find arguments for functions at base type and create scrutinees for match statements.
    member private this.AdvanceBase(b:base_judgment_type) : base_judgment_type =
        // HEURISTIC: Eta-long sidestepping.
        // Use unfocused terms from the context to fill holes, avoiding enumeration.  This
        // strategy is especially useful in cases where it would be expensive to enumerate
        // deep enough to reach the eta-long version of a function (\x. \y. sum x y) but
        // very cheap to guess the non-eta-long version (sum).
        let sidestepping_pool = this.Seq.Unfocused
                                  .unfilter(fun x -> x.t.IsBase || x.t.IsUnit || x.t.IsPoly)
        let sidestep_one((sk, h, depth, decIn):base_subproblem) : skeleton list =
            // We should only try structurally decreasing arguments that have the right type.
            sidestepping_pool
              .filter(fun x -> x.t = sk.HoleType(h))
              .filter(fun x -> x.StructuralCheck(decIn))
              .map(fun x -> sk.SolveHole(h, HC.evar x.name, x.rs))
              .somes()

        // HEURISTIC: Raw term enumeration.
        // Enumerate expressions at the type of a hole and use them to attempt to solve the hole.
        // We use typechecking to see whether the generated expression satisfies the refinements.
        // It cannot make use of the libraries, since we don't have refinements for them.
        let enumerate_one((sk, h, depth, decIn):base_subproblem) : skeleton list =
            let enum_with_disjunction =
                match this.Rep.enum_strategy with
                | EnumRaw -> false
                | EnumUnionBase -> sk.HoleType(h).IsBase
                | EnumUnionEverywhere  -> true
                | EnumUnionEverywhereUnsound -> true
            let unsound_or =
                match this.Rep.enum_strategy with
                | EnumUnionEverywhereUnsound -> true
                | _                          -> false
            let sqnt =
                if enum_with_disjunction then
                    let rs = sk.GetHole(h, unsound_or=unsound_or)
                    this.Seq.SetGoalRefns(rs).SetGoalType(sk.HoleType h).SetDecIn(decIn)
                else
                    this.Seq.NoRefns().SetGoalType(sk.HoleType h).SetDecIn(decIn)
            let args = GenR.GenR(sqnt, depth, {jit=false; enum_strategy=this.Rep.enum_strategy})
            (List.ofSeq args).map(fun e -> sk.SolveHoleCheck(h, e, this.Seq.RefnGam)).somes()

        // HEURISTIC: Just-in-time library sampling.
        // If just-in-time sampling is enabled, sample libraries and evalute them to see
        // whether the results match our goal refinements in every world.
        let from_library =
            if not this.Rep.jit then Seq.empty
            else SampleJit.Solve(this.Rep.seq, this.Rep.depth)
        let scrut_cap = Math.Min(this.Rep.scrut_size, this.Rep.depth)
        let from_library_scruts =
            if not this.Rep.jit || b.last_scrut_size >= scrut_cap || this.Rep.no_matches then []
            else [b.last_scrut_size+1..scrut_cap]
                    .collect(fun n -> SampleJit.Scrutinees(this.Rep.seq, n))

        // Prepare a subproblem for the first hole of a skeleton.
        let prepare_subproblem(s:skeleton) : base_subproblem =
            let h = s.FirstHole
            let dec = if s.IsRecFn && s.FirstArg.IsSome && s.FirstArg.Value = h
                        then Some s.Name else None
            (s, h, 0, dec)

        // Apply a heuristic
        let use_heuristic(heuristic, on:list<base_subproblem>) : skeleton list * base_subproblem list =
            let (unsolved, solved) = List.partition skeleton.HasHoleFn (on.collect(heuristic))
            (solved, unsolved.map(prepare_subproblem))
        // Repeatedly applies a heuristic.  Returns those expressions that are solved and
        // those that have had at least one hole solved using the heuristic.
        let repeat_heuristic(heuristic, on:list<base_subproblem>)
            : skeleton list * base_subproblem list =
            let mutable on = on
            let mutable (solved', on') = ([], [])
            while not on.IsEmpty do
                let (solved, unsolved) = use_heuristic(heuristic, on)
                on'     <- on' @ unsolved
                solved' <- solved' @ solved
                on      <- unsolved
            (solved', on')

        // Advance elimination forms.
        let advance_elims(enum_cap:int) (off:skeleton list) (on:base_subproblem list)
            : skeleton list * base_subproblem list * skeleton list =
            // Move new expressions into consideration.
            let (not_off, off') = List.partition (fun (sk:skeleton) -> sk.Size <= enum_cap) off
            
            // Any new expressions without holes are done.
            let (unsolved, solved) = List.partition skeleton.HasHoleFn not_off
            let unsolved = unsolved.map(prepare_subproblem)

            // Advance sidestepping as far as possible.
            let (solved2, unsolved2) = repeat_heuristic(sidestep_one, unsolved)

            // Begin enumerating.
            let mutable solved' = solved @ solved2
            let mutable on      = unsolved @ unsolved2 @ on
            let mutable on'     = []

            while not on.IsEmpty do
                // Determine which subproblems can currently be advanced.  Add those that cannot
                // to on'.  Advance the enumeration depth of those that can be enumerated and
                // place them back in on.
                let (do_enum, dont_enum) =
                    List.partition (fun (sk:skeleton,_,n,_) -> sk.Size + n <= enum_cap) on
                on' <- on' @ dont_enum
                on  <- do_enum.map(fun (sk,h,n,dec) -> (sk,h,n+1,dec))

                // Enumerate once.
                let (do_enum_unsolved, do_enum_solved) =
                    List.partition skeleton.HasHoleFn (do_enum.collect(enumerate_one))
                let do_enum_unsolved = do_enum_unsolved.map(prepare_subproblem)

                // Add those expressions that are solved to solved'.
                // Add those that remain unsolved to on.
                solved' <- solved' @ do_enum_solved
                on <- on @ do_enum_unsolved

                // Apply sidestepping to those that remain unsolved.
                let (solved2, unsolved2) = repeat_heuristic(sidestep_one, do_enum_unsolved)
                solved' <- solved' @ solved2
                on <- on @ unsolved2

            (off', on', solved')

        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // SYNTH-SUM-R (CONSTRUCTORS).
        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // Handle constructor refining if necessary.
        b.ctors.iter(judgment.AdvanceFn)

        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // SYNTH-CTX (VARIABLES, PROJECTION, AND APPLICATION).
        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        let (off', on', solved') = advance_elims this.Rep.depth b.off b.on
        let solved' = solved'.map(fun sk -> sk.Body) @ b.solved @ (Seq.toList from_library)

        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // SYNTH-SUM-L (MATCH STATEMENTS).
        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        let (m_off', m_on', scruts) =
            if this.Rep.matches <= 0 || this.Rep.no_matches then (b.m_off, b.m_on, [])
            else advance_elims scrut_cap b.m_off b.m_on
        let from_lib = if this.Rep.matches <= 0 then []
                       else from_library_scruts.map(this.SynthSumL).somes()
        let scruts = scruts.filter(fun sk -> sk.GoalContainsAllCtors())

        // Create match statement problems for new scrutinees.
        let make_match (sk:skeleton) = this.SynthSumL(sk.Body, sk.Info, sk.GoalType, sk.GoalRefns)
        let matches' = b.matches @ scruts.map(make_match).somes() @ from_lib
        matches'.iter(fun mtch -> mtch.branches.iter(judgment.AdvanceFn))

        // Finish the update.
        {ctors = b.ctors; last_scrut_size=scrut_cap;
         on   =   on';   off   = off'; solved  = solved'
         m_on = m_on'; m_off = m_off'; matches = matches'}
                
    static member private AdvanceFn(j:judgment) = j.Advance()
    member public this.Advance() : unit =
        // Refine.
        if not(this.DoneRefining) then this.Refine()

        // Recursively step.
        match this.Rep.rule with
        | JUndetermined | JImpossible | JSolved _ | JUnit -> ()
        | JCtor(j, _)      -> j.Advance()
        | JFunc(j, _, _)   -> j.Advance()
        | JTuple(js, _)    -> js.iter(judgment.AdvanceFn)
        | JOr js           -> js.iter(judgment.AdvanceFn)
        | JBase b          -> this.Rep.rule <- JBase (this.AdvanceBase b)

    // Convert lists of old and new sequences into lists of items whose combinations are new.
    static member private mix(olds:list<seq<'t>>, news:list<seq<'t>>) : seq<list<'t>> =
        // Enumerate all combinations of olds and news. Make sure we don't enumerate
        // any sequences of all olds.
        let cartesians =
            let fold_fn (b : seq<list<bool>>) _ : seq<list<bool>> =
                seq{
                    yield! Seq.map (fun ls -> false :: ls) b
                    yield! Seq.map (fun ls -> true  :: ls) b
                }
            let b  = Seq.append (Seq.singleton [false])
                                (Seq.singleton [true ])
            Seq.fold fold_fn b (Seq.ofList [1..olds.Length - 1])

        let results = Seq.filter (List.forall (fun v -> v = false)) cartesians
                        |> Seq.map (List.map3 (fun n o b -> if b then n else o) news olds)
                        |> Seq.collect Seq2.cartesianList

        results

    // Enumerate solutions.  Calling enumerate a second time without stepping should yield nothing.
    // The public version of Reap.  Materializes the sequence so that nothing unexpected happens
    // later.
    member public this.Reap() : list<expr> =
        Seq.toList (this.ReapLazy())

    // Private version of reap.  ReapLazy creates sequences lazily (which makes appends
    // faster when they're eventually materialized).
    static member private ReapLazyFn(j:judgment) = j.ReapLazy()
    member private this.ReapLazy() : seq<expr> =
        let out = 
            match this.Rep.rule with
            | JUndetermined | JImpossible -> Seq.empty
            | JSolved e -> Seq.singleton e
            | JUnit -> if this.Rep.enum then this.MarkImpossible()
                       Seq.singleton HC.eunit
            | JCtor(j, c) ->
                let map_fn (e:expr) = HC.ector(c, e)
                Seq.map map_fn (j.ReapLazy())
            | JFunc(j, x, isRec) ->
                let map_fn (e:expr) =
                  if isRec.IsNone then
                     HC.efun {argl=x; arg_type=fst this.Seq.GoalType.Arr; body=e}
                  else
                     HC.efix {argf=x; name=isRec.Value; arg_type=fst this.Seq.GoalType.Arr; body=e
                              ret_type=snd this.Seq.GoalType.Arr}
                Seq.map map_fn (j.ReapLazy())
            | JTuple(js, olds) -> 
                let news  = js.map(judgment.ReapLazyFn)
                let out   = judgment.mix(olds, news)
                let olds' = List.map2 Seq.append olds news
                this.Rep.rule <- JTuple(js, olds')
                Seq.map HC.etup out

            | JOr js -> Seq.collect (judgment.ReapLazyFn) js

            | JBase b ->
                // Match statements.
                let handle_match (mtch:match_entry) =
                    let news  = mtch.branches.map(judgment.ReapLazyFn)
                    let out   = judgment.mix(mtch.solved, news)
                    let olds' = List.map2 Seq.append mtch.solved news

                    let prepare(es : expr list) =
                        let (cs, xs) = List.unzip mtch.patterns
                        HC.ematch(mtch.scrut, List.zip3 cs xs es)
                    (Seq.map prepare out, {mtch with solved = olds'})

                let (matches_out, matches') =
                    let (outs, matches') = List.unzip (b.matches.map(handle_match))
                    (Seq.concat (Seq.ofList outs), matches')

                // Solve constructors and elimination forms.
                this.Rep.rule <- JBase {off=b.off; on=b.on; solved=[];
                                        m_off=b.m_off; m_on=b.m_on; matches=matches';
                                        ctors=b.ctors; last_scrut_size=b.last_scrut_size}
                matches_out.append(Seq.collect judgment.ReapLazyFn b.ctors)
                           .append(Seq.ofList b.solved)
        let out = Seq.cache out
        if not(out.IsEmpty) && not(this.Rep.enum) then
            let out_hd = Seq.head out
            this.Rep.rule <- JSolved out_hd
            Seq.singleton out_hd
        else
            out

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // PRINTING.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    override this.ToString() =
        let rec to_string (n:int) (p:string) (j:judgment) =
            let indent  = String.replicate n     "  "
            let indent1 = String.replicate (n+1) "  "
            let out  s = sprintf "%s%s[%d] %s\n" indent p j.Rep.depth s
            let out1 s = sprintf "%s%s[%d] %s\n" indent1 p j.Rep.depth s

            if this.Rep.depth <= 0 then ""
            else
                match j.Rep.rule with
                | JSolved e        -> out (sprintf "%O" e)
                | JImpossible      -> out "Impossible"
                | JUndetermined    -> out "Undetermined"
                | JCtor(j, c)      -> out (sprintf "%s(*)" c) + to_string (n+1) "*" j
                | JFunc(j, x, f)   -> if f.IsNone then
                                          out (sprintf "fun %s -> *" x) + to_string (n+1) "*" j
                                      else
                                          out (sprintf "fix %s %s -> *" f.Value x) + to_string (n+1) "*" j
                | JTuple(js,_)     -> let indices = [1..js.Length].map(sprintf "%d")
                                      out (sprintf "(%s)" (String.concat ", " indices)) +
                                      String.concat "" (List.map2 (fun i j -> to_string (n+1) i j) indices js)
                | JUnit            -> out "()"
                | JOr js           -> String.concat "" (List.map (fun j -> to_string n p j) js)
                | JBase b          ->
                    let ctor = String.concat "" (List.map (to_string n p) b.ctors)
                    let print_on (s:skeleton, h:hole, _,  _) = out (sprintf "on: %O" s.Body)
                    let on    = String.concat "" (List.map print_on b.on)
                    let off   =
                        String.concat "" (List.map (fun (s:skeleton) -> out (sprintf "off: %O" s.Body)) b.off)

                    let print_matches (m:match_entry) =
                        let indices = [1..m.patterns.Length].map(sprintf "%d")
                        let bss = List.map2 (fun (c, x) n -> sprintf "|%s %s %s" n c x) m.patterns indices
                        let s = out (sprintf "match %O with %s" m.scrut (String.concat " " bss))
                        let t = List.map2 (fun i j -> to_string (n+1) i j) indices m.branches
                        s + String.concat "" t
                    let matches = String.concat "" (b.matches.map(print_matches))
                    ctor + off + on + matches
        to_string 0 "*" this