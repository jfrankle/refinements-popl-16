module Synth.Skeleton
open Util
open Lang
open Refns
open Typecheck

// Holes.
type private case = int
type private holes_map = Map<world, Map<case, Map<hole, refn>>>
type private goals_map = Map<world, Map<case, refn>>

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// A SKELETON EXPRESSION.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// The representation of a skeleton.
type private skeleton_rep = {
    body       : expr
    holes      : holes_map
    holest     : Map<hole, typ>
    fresh_hole : int
    fresh_case : Map<world, case>
    goalt      : typ
    goalrs     : goals_map
    first_arg  : hole option
    info       : id_info
}
type skeleton private (rep:skeleton_rep) =
    member private this.Rep = rep

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // CREATE A SKELETON.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    static member public Create(x:id_info) =
        let rs = x.rs.mapv(fun r -> Map.empty.Add(0, r))
        skeleton({
                   info   = x
                   goalt  = x.t
                   goalrs = rs
                   body   = HC.evar x.name
                   holes  = rs.mapv(make (Map.empty.Add(0, Map.empty)))
                   holest = Map.empty
                   fresh_hole = 0
                   fresh_case = rs.mapv(make 1)
                   first_arg  = None
        })

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // HASHING AND EQUALITY
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    member private this.HashCode =
        let result = 17
        let result = result * 31 + hash this.Rep.goalrs
        let result = result * 31 + hash this.Rep.holes
        let result = result * 31 + hash this.Rep.body 
        result                                   
    override this.GetHashCode() = this.HashCode
    override this.Equals(thatobj) =
        match thatobj with
        | :? skeleton as that -> this.Rep.body = that.Rep.body &&
                                 this.Rep.holes = that.Rep.holes &&
                                 this.Rep.goalrs = that.Rep.goalrs
        | _ -> false
        
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // PROPERTIES, DUPLICATING WORLDS, AND DELETING WORLDS.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    static member public HasHoleFn(sk:skeleton) = sk.HasHole

    member public this.GoalType  with get() = this.Rep.goalt
    member public this.Name      with get() = this.Rep.info.name
    member public this.Nodes     with get() = this.Rep.body.Nodes
    member public this.Size                 = this.Rep.body.Size
    member public this.Body                 = this.Rep.body
    member public this.Info                 = this.Rep.info
    member public this.IsRecFn              = this.Rep.info.about.IsRecFn
    member public this.FirstArg  with get() = this.Rep.first_arg
    member public this.HasHole   = not(this.Rep.holest.IsEmpty)
    member public this.FirstHole = this.Rep.holest.FirstKey
    member public this.HoleCount = this.Rep.holest.Count
    member public this.HoleType(h:hole) = this.Rep.holest.[h]
    member public this.Holes    with get() = this.Rep.holest.Keys
    member public this.GoalRefns =
        this.Rep.goalrs.mapv(fun m -> m.Values.PushAnd(this.GoalType))
    member public this.StructuralCheck(mustBeDecIn:id option) =
        this.Rep.info.StructuralCheck(mustBeDecIn)

    member public this.NoRefns() =
        skeleton({this.Rep with fresh_case = Map.empty;
                                goalrs = Map.empty;
                                holes = Map.empty})

    // Duplicate and delete worlds.
    member public this.DuplicateWorld(w, w') =
        skeleton({ this.Rep with
                    holes      = this.Rep.holes.Copy(w, w')
                    goalrs     = this.Rep.goalrs.Copy(w, w')
                    fresh_case = this.Rep.fresh_case.Copy(w, w')
                 })

    member public this.DeleteWorld(w) =
        skeleton({ this.Rep with
                    holes      = this.Rep.holes.Remove(w)
                    goalrs     = this.Rep.goalrs.Remove(w)
                    fresh_case = this.Rep.fresh_case.Remove(w)
                 })

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // SOLVING HOLES
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Do the goal of the cases across all worlds collectively contain all constructors?
    member public this.GoalContainsAllCtors() : bool =
        if Map.exists (fun _ m -> Map.exists (fun _ (r:refn) -> r.IsBTop) m) this.Rep.goalrs then
            true
        else
            let ctors_in_goal =
                let fold_fn (s:Set<id>) _ (m:Map<case, refn>) =
                    let fold_fn (s:Set<id>) _ (r:refn) =
                        Map.fold (fun (s:Set<id>) (c:id) _ -> s.Add(c)) s r.Base
                    Map.fold fold_fn s m
                Map.fold fold_fn Set.empty this.Rep.goalrs
            let all_ctors = Ctors.AllCtors(this.GoalType.Base).map(fun (c:ctor) -> c.name) |> Set.ofList
            all_ctors.IsSubsetOf(ctors_in_goal)

    // Get the refinement specification for a hole, which includes disjunction.
    member public this.GetHole(h:hole, ?unsound_or:bool) : refns =
       let t = this.Rep.holest.[h]
       let unsound_or = defaultArg unsound_or false
       if unsound_or then
           this.Rep.holes.map(fun w m -> m.mapv(Map.find h).Values.PushOrUnsound(t))
       else
           this.Rep.holes.map(fun w m -> m.mapv(Map.find h).Values.PushOr(t))

    // Preserve only those cases that subrefine the argument refns in each world.
    member public this.ConformTo(rs:refns) : skeleton option =
        // Find all cases in each world that match the goal.
        let sub (r1:refn) (r2:refn) = r1.SubRefines(r2, this.Rep.goalt)
        let traces = this.Rep.goalrs.map(fun w (m:Map<_, _>) ->
                                           m.filterv(fun (r1:refn) ->
                                               sub r1 rs.[w]).Keys |> Set.ofList)

        let goals' = this.Rep.goalrs.map(fun w m -> m.filter(fun c _ -> traces.[w].Contains(c)))
        let holes' = this.Rep.holes .map(fun w m -> m.filter(fun c _ -> traces.[w].Contains(c)))

        if Map.exists (fun w (m:Map<_, _>) -> m.IsEmpty) goals' then None
        else Some (skeleton({this.Rep with goalrs = goals'; holes = holes'}))

    // Use an expression and its refinements to solve a hole.
    member public this.SolveHole(h:hole, e:expr, rs:refns) : skeleton option =
       let t = this.Rep.holest.[h]
       let holes = this.Rep.holes.map(fun w m -> m.filterv(fun m -> rs.[w].SubRefines(m.[h], t)))
                                 .map(fun w m -> m.mapv(fun m -> m.Remove(h)))
       let goals = this.Rep.goalrs.map(fun w m -> m.filter(fun c _ -> holes.[w].ContainsKey(c)))
       let holest = this.Rep.holest.Remove(h)
       let body   = this.Rep.body.FillHoles(Map.empty.Add(h, e))

       if Map.exists (fun w (m:Map<_, _>) -> m.IsEmpty) goals then None
       else Some (skeleton({this.Rep with goalrs = goals; holes = holes; holest = holest; body=body}))

    // Solve a hole by providing an expression.  It typechecks the expression against the hole
    // to ensure that it is a solution.
    member public this.SolveHoleCheck(h:hole, e:expr, gam:world -> Map<id, refn * typ>)
       : skeleton option =
       let t = this.Rep.holest.[h]

       // Typecheck the expression against every case in every world for hole h.
       // If any world ends up with no cases, the result will be None.
       let fold_fn (holes':holes_map option) w (m:Map<case, Map<hole, refn>>) =
           if holes'.IsNone then None
           else
               let m' = m.filterv(fun m -> e.RefnCheck(gam w, m.[h], t))
               if m'.IsEmpty then None
               else Some (holes'.Value.Add(w, m'.mapv(fun m -> m.Remove(h))))
       
       // If some world has no cases, then return None
       let holes = Map.fold fold_fn (Some Map.empty) this.Rep.holes
       if holes.IsNone then None

       // Otherwise, update the skeleton.
       else
           let holes = holes.Value
           let goals = this.Rep.goalrs.map(fun w m -> m.filter(fun c _ -> holes.[w].ContainsKey(c)))
           let holest = this.Rep.holest.Remove(h)
           let body   = this.Rep.body.FillHoles(Map.empty.Add(h, e))

           if Map.exists (fun w (m:Map<_, _>) -> m.IsEmpty) goals then None
           else Some (skeleton({this.Rep with goalrs = goals; holes = holes; holest = holest; body=body}))

    // Search for all identical refinements that appear across all worlds in a hole.
    member public this.SolveHoleForConstant(h:hole) : refn list =
        // Check to see whether there are any refinements that appear in every world
        // for the hole.
        let bs = this.Rep.holes.[this.Rep.holes.FirstKey].mapv(fun m -> m.[h])
        let filter_fn (r:refn) =
            Map.forall (fun _ (cs:Map<case, Map<hole, refn>>) ->
                           Map.exists (fun _ (hs:Map<hole, refn>) -> hs.[h] = r) cs) this.Rep.holes
        bs.filterv(filter_fn).Values
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // FOCUSING.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // SYNTH-INTERSECTION-L.
    member private this.SynthIntersectionL() : skeleton =
        if not(this.GoalType.IsArr) then failwith "Only arrows can contain intersection."

        let new_goals : goals_map ref       = ref Map.empty
        let new_holes : holes_map ref       = ref Map.empty
        let new_fc    : Map<world, int> ref = ref this.Rep.fresh_case

        let handle_world (w:world) (cs:Map<case, refn>) =
            // Add the world to new_goals and new_holes.
            new_goals := new_goals.Value.Add(w, Map.empty)
            new_holes := new_holes.Value.Add(w, Map.empty)

            // Split the intersected goal refinements within this world into cases.
            let split_case (c, r:refn) = r.Arr.AsList.map(fun rss -> (c, HC.narr(CNF.Create([rss]))))
            let split_cases = cs.ToList().collect(split_case)

            // Create a new case for each individual case.
            let handle_case (c:case, r:refn) =
                let c' = new_fc.Value.[w]
                new_goals := (!new_goals).Add(w, (!new_goals).[w].Add(c', r))
                new_holes := (!new_holes).Add(w, (!new_holes).[w].Add(c', this.Rep.holes.[w].[c]))
                new_fc    := (!new_fc   ).Add(w, new_fc.Value.[w] + 1)

            List.iter handle_case split_cases

        // Execute handle_world.
        Map.iter handle_world this.Rep.goalrs
        skeleton({ this.Rep with goalrs = !new_goals; holes = !new_holes; fresh_case = !new_fc })
    
    // SYNTH-UNION-L.
    member private this.SynthUnionL(duplicate_world : world -> world, delete_world : world -> unit)
        : skeleton =
        if not(this.GoalType.IsArr  || this.GoalType.IsTup ||
               this.GoalType.IsBase || this.GoalType.IsPoly) then
            failwith "Can only break up unions at tuple, base, polymorphic, or arrow type"

        // Code shared by tuple and arrow.
        let synth_union_l (broken_goals:Map<world, Map<case, 't list>>) =
            let new_goals : Map<world, Map<case, 't>> ref = ref Map.empty
            let new_holes : holes_map ref                 = ref Map.empty
            let new_fc    : Map<world, int> ref           = ref Map.empty

            let handle_world (w:world) (cs:Map<case, 't list>) =
                let handle_new_case (m:Map<case, 't>) =
                    let w' = duplicate_world(w)
                    new_goals := (!new_goals).Add(w', m)
                    new_holes := (!new_holes).Add(w', this.Rep.holes.[w])
                    new_fc    := (!new_fc   ).Add(w', this.Rep.fresh_case.[w])

                // Break up the disjunction in the cases of this world, take the cartesian
                // products of the cases, and produce sets of cases for new worlds.
                cs.Cartesian().iter(handle_new_case)

                // Delete the old world.
                delete_world(w)

            // Execute handle_world.
            Map.iter handle_world broken_goals
            (!new_goals, !new_holes, !new_fc)

        // The arrow case.
        if this.GoalType.IsArr then
            let broken_goals = this.Rep.goalrs.mapv(Map.map (fun _ (r:refn) -> r.Arr.Or))
            let (new_goals, new_holes, new_fc) = synth_union_l broken_goals
            let new_goals = new_goals.mapv(Map.map (fun _ t -> HC.narr(CNF.Create1(t))))
            skeleton({ this.Rep with goalrs = new_goals; holes = new_holes; fresh_case = new_fc})
        // The base case and polymorphic variable case.
        elif this.GoalType.IsBase || this.GoalType.IsPoly then
            let broken_goals = this.Rep.goalrs.mapv(Map.map (fun _ (r:refn) -> r.PullOr() |> Seq.toList))
            let (new_goals, new_holes, new_fc) = synth_union_l broken_goals
            skeleton ({ this.Rep with goalrs = new_goals; holes = new_holes; fresh_case = new_fc })
        // The tuple case.
        else
            let broken_goals = this.Rep.goalrs.mapv(Map.map (fun _ (r:refn) -> r.Tup))
            let (new_goals, new_holes, new_fc) = synth_union_l broken_goals
            let new_goals = new_goals.mapv(Map.map (fun _ t -> HC.ntup [t]))
            skeleton({ this.Rep with goalrs = new_goals; holes = new_holes; fresh_case = new_fc})

    // SYNTH-PRODUCT-L
    member public this.SynthProductL(duplicate_world : world -> world, delete_world : world -> unit)
        : skeleton list =

        // 1) Break up any disjunction.
        let skel = this.SynthUnionL(duplicate_world, delete_world)

        // 2) Break up the tuples themselves.
        let arity = skel.Rep.goalt.Tup.Length
        let old_goals = skel.Rep.goalrs.mapv(Map.map (fun _ (r:refn) -> r.Tup.Singleton))

        // Create the projections.
        let handle_world (new_goals : list<goals_map>) (w:world) (cs:Map<case, rtup>)
            : list<goals_map> =
            let handle_case (new_cases : list<Map<case, refn>>) (c:case) (rt:rtup) =
                List.map2 (fun (m:Map<case, refn>) (r:refn) -> m.Add(c, r)) new_cases rt
            let new_cases = Map.fold handle_case (List.replicate arity Map.empty) cs
            List.map2 (fun (m:goals_map) (c:Map<case, refn>) -> m.Add(w, c)) new_goals new_cases
        let new_goals = Map.fold handle_world (List.replicate arity Map.empty) old_goals

        let produce_skeleton (i:int) (t:typ) (g:goals_map) =
            skeleton({ skel.Rep with
                          goalt  = t
                          goalrs = g
                          body   = HC.eproj(i+1, skel.Rep.body)
                     })
        List.mapi2 produce_skeleton skel.Rep.goalt.Tup new_goals

    // SYNTH-IMPL-L.
    member public this.SynthImplL(duplicate_world : world -> world, delete_world : world -> unit)
        : skeleton =
        // 1) Break up all intersection and union.
        let skel = this.SynthIntersectionL()
        let skel = if skel.Rep.goalrs.existsv(fun m ->
                           m.existsv(fun r -> List.exists (fun (ls:list<_>) -> ls.Length > 1) r.Arr.AsList)) then
                     skel.SynthUnionL(duplicate_world, delete_world)
                   else skel

        // 2) Break up all arrows.
        let h'       = skel.Rep.fresh_hole
        let (t1, t2) = skel.Rep.goalt.Arr

        let new_goals : goals_map ref = ref Map.empty
        let new_holes : holes_map ref = ref Map.empty

        let handle_world (w:world) (cs:Map<case, refn>) =
            new_goals := (!new_goals).Add(w, Map.empty)
            new_holes := (!new_holes).Add(w, Map.empty)

            let handle_case (c:case) (r:refn) =
                let (r1, r2) = r.Arr.Singleton
                new_goals := (!new_goals).Add(w, (!new_goals).[w].Add(c, r2))
                new_holes := (!new_holes).Add(w, (!new_holes).[w].Add(c, skel.Rep.holes.[w].[c].Add(h', r1)))
            cs.iter(handle_case)

        // Create the output skeleton.
        skel.Rep.goalrs.iter(handle_world)
        skeleton({ skel.Rep with
                     first_arg  = if this.Rep.first_arg.IsNone then Some h' else this.Rep.first_arg
                     fresh_hole = h' + 1
                     goalrs     = !new_goals
                     holes      = !new_holes
                     holest     = skel.Rep.holest.Add(h', t1)
                     goalt      = t2
                     body       = HC.eapp(skel.Rep.body, HC.ehole h')
                 })

    // SYNTH-UNIT-L.
    // Any world that has unit-bottom in the goal should be discarded.  It's equivalent
    // to having false on the left.  Afterward, the entire term is discarded, since
    // it's equivalent to having true on the left.
    member public this.SynthUnitL(delete_world : world -> unit) : unit =
        let worlds_to_del = this.Rep.goalrs.filterv(Map.exists (fun _ (r:refn) -> r.IsUBot)).Keys
        worlds_to_del.iter(delete_world)

    // SYNTH-BASE-L.
    // Any world that has base-bottom in the goal should be discarded.
    member public this.SynthBaseL(duplicate_world : world -> world,
                                  delete_world : world -> unit) : skeleton =
        this.SynthUnionL(duplicate_world, delete_world)

    // SYNTH-POLY-L.
    // Any world that has base-polymorphic in the goal should be discarded.
    member public this.SynthPolyL(duplicate_world : world -> world,
                                  delete_world : world -> unit) : skeleton =
        this.SynthUnionL(duplicate_world, delete_world)