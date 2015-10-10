module Synth.SampleJit
open Util
open Lang
open Refns
open Sequent
open SkeletonT
open Eval

let GenFn : (id_info_t list * typ * int * id option -> seq<expr>) ref = ref (make Seq.empty)

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// LAZILY CACHE LIBRARY CONTEXTS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let mutable lib_info = lazy(Library.Info())
let mutable lib_env  = lazy(Library.Env())
let mutable lib_ctx  = lazy(Library.Gam().ToList()
                                .collect(fun (x, t) -> 
                                    skeleton_t.Create({name=x; t=t; about=AIUninteresting})))

// For each library skeleton...
let generate (sequ:sequent) (info:id_info_t list) (depth:int) (sk:skeleton_t) : seq<expr> =
    if not sk.HasHole && sk.Nodes = depth then Seq.singleton sk.Body
    else
        // Generate all tuples at size {depth} such that the elements of the
        // tuple line up with the holes.  This saves us a little work in pushing
        // around the depth and diagonalizing, since Gen does it for us.
        let holes = sk.Holes
        let t = HC.ttup(holes.map(sk.HoleType))
        let es = GenFn.Value(info, t, depth-sk.Nodes, sequ.DecIn)
        let es2 = List.ofSeq es

        // Fill the holes to produce expressions.
        let handle_expr (e:expr) =
            match e.Node with
            | ETup es -> sk.SolveHoles((List.zip holes es).ToMap())
            | _       -> failwith "Bad."
        es.map(handle_expr).map(fun (sk:skeleton_t) -> sk.Body)

// Use the library to solve the goal of the sequent at the given depth.
let public Solve(sequ:sequent, depth:int) =
    // Create the gamma and environment for evaluation.
    let info = lib_info.Value @ (sequ.Unfocused.map(LangExtensions.ToInfoT))
    let eval_gam = info.map(fun x -> (x.name, x.t)).ToMap()
    let eval_env w =
        List.fold (fun (m:Map<_,_>) (x:id_info) ->
            m.Add(x.name, HC.vrefn x.rs.[w])) lib_env.Value sequ.Unfocused

    // Collect all solved skeletons, evaluate them to a value in every world, and check
    // to see whether they solve the synthesis problem.
    let can_solve (e:expr) : bool =
        let try_in_world (w:world) (goalr:refn) =
            let ro = try (e.Eval eval_gam (eval_env w) sequ.GoalType).TryRefn with _ -> None
            ro.IsSome && ro.Value.SubRefines(goalr, sequ.GoalType)
        Map.forall try_in_world sequ.GoalRefns

    let exprs = lib_ctx.Value.filter(fun sk -> sk.Type = sequ.GoalType)
                |> Seq.ofList |> Seq.collect (generate sequ info depth)
    let exprs = exprs.filter(can_solve)
    exprs

// Use the library to produce match statement scrutinees.
let public Scrutinees(sequ:sequent, depth:int) =
    // Create the gamma and environment for evaluation.
    let info = lib_info.Value @ (sequ.Unfocused.map(LangExtensions.ToInfoT))
    let eval_gam = info.map(fun x -> (x.name, x.t)).ToMap()
    let eval_env w =
        List.fold (fun (m:Map<_,_>) (x:id_info) ->
            m.Add(x.name, HC.vrefn x.rs.[w])) lib_env.Value sequ.Unfocused

    // Collect all solved skeletons, evaluate them to a value in every world,
    // and build refinements
    let can_solve (e:expr, x:id, t:typ, decIn:id option) : (expr * id_info * typ * refns) option =
        let rec build_refns (ls:world list) =
            match ls with
            | [] -> Some Map.empty
            | w :: tl ->
                let ro = try (e.Eval eval_gam (eval_env w) t).TryRefn with _ -> None
                if ro.IsNone then None
                else
                    match build_refns tl with
                    | None -> None
                    | Some m -> Some (m.Add(w, ro.Value))
        match build_refns sequ.GoalRefns.Keys with
        | None -> None
        | Some rs -> if not (rs.ContainsAllCtors(t)) then None
                     else Some (e, {name=x;t=t;rs=rs;about=if decIn.IsNone then AIUninteresting
                                                           else AIDecIn decIn.Value }, t, rs)

    let exprs = lib_ctx.Value
                |> Seq.ofList
                |> Seq.collect (fun sk -> (generate sequ info depth sk)
                                           .map(fun e -> (e, sk.Name, sk.Type, sk.DecIn)))
    (Seq.toList exprs).map(can_solve).somes()