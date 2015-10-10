module Synth.Normalize
open Lang
open Util
open Refns
open System.Runtime.CompilerServices

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// INHABITATION.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Checks whether a refinement is inhabited.
let rec private is_inhabited(r : refn, t : typ) : bool =
    let norm r t = normalize(r, t)

    let rec inhabited (r:refn) (t:typ) =
        match r.Node, t.Node with
        | NUTop,     _            -> true
        | NBTop _,   _            -> true
        | NPTop _,   _            -> true
        | NPoly ps,  _            -> not ps.IsEmpty
        | NTup rss,  TTup ts      -> List.exists (fun rs -> List.forall2 inhabited rs ts) rss
        | NArr cnf,  TArr(t1, t2) ->
            let exists_fn rs =
                let (rs1, rs2) = List.unzip rs
                let (subsets1, subsets2) = (rs1.Subsets(), rs2.Subsets())

                let check_subsets (subset1 : refn list) (subset2 : refn list) =
                    let (and1, and2) = (subset1.PushAnd(t1), subset2.PushAnd(t2))
                    not (inhabited and1 t1) || inhabited and2 t2

                List.forall2 check_subsets subsets1 subsets2
            cnf.DNF().FoldList(exists_fn, List.exists ident)
        | NBase rs, TBase(b, vs)  ->  Map.exists (fun c r -> inhabited r (Ctors.Type c vs)) rs
        | _,            _ -> failwith "Bad input."
        
    // Invoke the function.
    inhabited r t

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// NORMALIZATION.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and private normalize(r:refn_ext, t:typ) : refn =
    let rec norm (t:typ) (r:refn_ext) : refn =
      let out = 
        match r with
        | REUnit       -> HC.nutop
        | REBase b     -> if Ctors.HasType(b) then HC.nbtop b
                          elif Library.Gam().ContainsKey(b) then HC.nlib b
                          elif t.IsPoly then  HC.npoly (Set.singleton b)
                          else failwith (sprintf "%s is not a type, polymorphic variable, or binding" b)
        | RECtor(c, r) -> HC.nbase (Map.empty.Add(c, norm (Ctors.Type c (snd t.Base)) r))
        | REArrA(a, b) -> HC.narr  (CNF.Create1(norm (fst t.Arr) a, norm (snd t.Arr) b))
        | REArrE(a, b) -> let (a, b) = (norm (fst t.Arr) a, norm (snd t.Arr) b)
                          if not(a.IsSingleton) then failwith "We need to create a unification var."
                          HC.narr(CNF.Create1(a, b))
        | RETup rs     -> HC.ntup[List.map2 norm t.Tup rs]
        | REOr  rs     -> rs.map(norm t).PushOr(t)
        | REAnd rs     -> rs.map(norm t).PushAnd(t)
        | REPTop p     -> HC.nptop p
        | RENot _ -> failwith "Cannot normalize 'not'"
      out
    norm t r 

and private simplify(r:refn, t:typ) : refn =
    let rec simpl(r:refn) (t:typ) : refn option =
        match r.Node with
        | NUBot -> None
        | NUTop | NBTop _ | NPoly _ | NPTop _ | NLib _ -> Some r
        | NBase m ->
            let m' = m.map(fun c r -> simpl r (Ctors.Type c (snd t.Base)))
                      .filterv(Option.isSome).mapv(Option.get)
            if m'.IsEmpty then None else Some (HC.nbase m')
        | NTup rs ->
            let map_fn (rs:rtup) =
                let rs' = List.map2 simpl rs t.Tup
                if List.exists Option.isNone rs' then None else Some (rs'.somes())
            let rs' = rs.map(map_fn).somes()
            if rs'.IsEmpty then None else Some (HC.ntup rs)
        | NArr cnf ->
            let (t1, t2) = t.Arr
            let arr_fn acc (r1, r2) =
                let (r1', r2') = (simpl r1 t1, simpl r2 t2)
                if r1'.IsNone then Choice1Of2(true)
                elif r2'.IsNone then acc
                else
                    match acc with
                    | Choice1Of2 true  -> acc
                    | Choice1Of2 false -> Choice2Of2([(r1'.Value, r2'.Value)])
                    | Choice2Of2 rs    -> Choice2Of2((r1'.Value, r2'.Value) :: rs) 
            let or_fn (rs:rarr list) = List.fold arr_fn (Choice1Of2(false)) rs
            let and_fn (rss:rarr list list) =
                let rss' = rss.map(or_fn)
                              .unfilter(function Choice1Of2 true -> true | _ -> false)
                if List.exists (function Choice1Of2 false -> true | _ -> false) rss' then None
                else
                    rss'.map(function Choice2Of2 rs -> rs | _ -> failwith "bad")
                    |> CNF.Create |> HC.narr |> Some
            and_fn cnf.AsList
    match simpl r t with
    | None -> t.Bot
    | Some r -> r


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// REFCON LIBRARIES.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let rec private subsitute_libraries(sample:value*typ -> refn option) (r:refn) : refn =
    match r.Node with
    | NUTop | NUBot | NBTop _ | NPTop _ | NPoly _ -> r
    | NTup rss -> HC.ntup (rss.map(List.map (subsitute_libraries sample)))
    | NArr cnf -> HC.narr (cnf.MapList((fun (a, b) -> (subsitute_libraries sample a, subsitute_libraries sample b)), ident))
    | NBase m  -> HC.nbase (m.mapv(subsitute_libraries sample))
    | NLib  x  -> let rc = sample (Library.Env().[x], Library.Gam().[x])
                  if rc.IsNone then failwith ("Library " + x + " was sampled into an empty " +
                                              "refinement.  Try increasing the sampling depth.")
                  else rc.Value
                     
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// NEGATION ELIMINATION.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Normalizes away negation.
let rec private neg_elim(r:refn_ext, t:typ) : refn_ext =
    let from_ext (r:refn_ext) (t:typ) = neg_elim(r, t)
    // Negate a refinement and call the normalization function.
    let negate_and_normalize (r : refn_ext) (t : typ) : refn_ext = from_ext (RENot r) t

    // Body of the function.
    match r, t.Node with
    // Non-negation rules.
    | REUnit, _ | REBase _, _ | REPTop _, _ -> r
    | RECtor(c, r'), _             -> RECtor(c, from_ext r' (Ctors.Type c (snd t.Base)))
    | REArrA(r1, r2), TArr(t1, t2) -> REArrA(from_ext r1 t1, from_ext r2 t2)
    | REArrE(r1, r2), TArr(t1, t2) -> REArrE(from_ext r1 t1, from_ext r2 t2)
    | RETup rs, TTup ts            -> RETup (List.map2 from_ext rs ts)
    | REAnd rs, _                  -> REAnd (List.map (fun r -> from_ext r t) rs)
    | REOr  rs, _                  -> REOr  (List.map (fun r -> from_ext r t) rs)

    // Negation rules.
    | RENot(REUnit), _ | RENot(REBase _), _ -> REOr[]
    | RENot(RECtor(name, r')), TBase(s, vs) ->
        let not_this_ctor = RECtor (name, negate_and_normalize r' (Ctors.Type name vs))
        let all_other_ctors = Ctors.AllCtors(s, vs)
                              |> List.filter (fun c -> not(name.Equals(c.name)))
                              |> List.map    (fun c -> RECtor (c.name, c.t.Top.External))
        REOr (not_this_ctor :: all_other_ctors)
    | RENot(REArrA(r1, r2)), TArr(t1, t2)   -> REArrE(from_ext r1 t1, negate_and_normalize r2 t2)
    | RENot(RETup rs), TTup ts              ->
        let unnegated  = List.map2 from_ext rs ts
        let negated    = List.map2 negate_and_normalize rs ts
        let patch k    = List.mapi (fun n l -> if k = n then List.nth negated k else l) unnegated
        REOr (List.mapi (fun n _ -> RETup (patch n)) rs)
    | RENot(REAnd rs), _ -> REOr  (List.map (fun r -> negate_and_normalize r t) rs)
    | RENot(REOr  rs), _ -> REAnd (List.map (fun r -> negate_and_normalize r t) rs)
    | RENot(RENot r'), _ -> from_ext r' t
    
    // Reject bad input.
    | RENot(_), _ -> failwith "Improperly negated term.  Can only negate singletons."
    | _, _ -> failwith "Bad normalization input."

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// EXTENSIONS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type refn_ext with
    member public r.NegElim  (t:typ) = neg_elim (r, t)
    member public r.Normalize(t:typ, sample:value * typ -> refn option) =
        subsitute_libraries sample (normalize(r, t))

type refn with
    member public r.Inhabited(t:typ) = is_inhabited(r, t)
    member public r.Simplify(t:typ)  = simplify(r, t)