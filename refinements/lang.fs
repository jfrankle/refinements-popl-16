module Synth.Lang
open System
open System.Runtime.CompilerServices
open Util

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// PRELIMINARIES.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type id       = string
type hole     = int
type world    = int
type pvar     = int
type arg_info =
   | AIUninteresting
   | AIDecIn of id
   | AIArgOf of id
   | AIRecFn

   member public this.DecIn   with get() = match this with AIDecIn f -> Some f | _ -> None
   member public this.ArgOf   with get() = match this with AIArgOf f | AIDecIn f -> Some f | _ -> None
   member public this.IsRecFn with get() = match this with AIRecFn -> true | _ -> false

type refns     = Map<world, refn>
and  id_info   = { name:id; t:typ; rs:refns; about:arg_info }
and  id_info_t = { name:id; t:typ; about:arg_info }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CONSTRUCTORS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and ctor = { name : id; t : typ; datatype : id } 

and Ctors private () =
    // Private state of the singleton module.
    static let mutable t_to_vars     : Map<id, id list>  = Map.empty
    static let mutable t_to_ctor     : Map<id, id list>  = Map.empty
    static let mutable ctor_to_info  : Map<id, id * typ> = Map.empty

    // Add a type.
    static member public AddType(type_name:id, type_vars:id list) =
        t_to_vars <- t_to_vars.Add(type_name, type_vars)

    // Add a constructor.
    static member public Add(ctor_name:id, type_name:id, t:typ) =
        t_to_ctor    <- t_to_ctor.Add(type_name, ctor_name :: t_to_ctor.[type_name, []])
        ctor_to_info <- ctor_to_info.Add(ctor_name, (type_name, t))

    // Check whether a datatype exists.
    static member public HasType(type_name:id) = t_to_ctor.ContainsKey(type_name)

    // Retrieve a constructor given a name.
    static member public GetCtor(ctor_name : id) (t_vars:typ list) =
        let (datatype, t) = ctor_to_info.[ctor_name]
        let t' = t.Subst(List.zip t_to_vars.[datatype] t_vars |> Map.ofList)
        {name = ctor_name; t = t'; datatype = datatype}

    // Retrieve all constructors for a given type.
    static member public AllCtors(datatype:id, t_vars:typ list) : ctor list =
        List.map (fun c -> Ctors.GetCtor c t_vars) t_to_ctor.[datatype]

    // Retrieve a type from a constructor name.
    static member public Type(ctor_name:id) (t_vars:typ list) =
        (Ctors.GetCtor ctor_name t_vars).t

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// POLYMORPHIC CONSTANTS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Registers all polymorphic constants used in the synthesis problem's refinements and
// the polymorphic types to which they correspond.
and PolyVars private () =
    // Maps each polymorphic constant to the polymorphic type it inhabits.
    static let mutable pvar_to_typ : Map<id, id> = Map.empty
    // Maps each polymorphic type to all of the polymorphic variables that inhabit it.
    static let mutable typ_to_pvar : Map<id, id list> = Map.empty

    // Registers a polymorphic variable with the type that it inhabits.
    static member public Add(pvar : id, t : id) =
        pvar_to_typ <- pvar_to_typ.Add(pvar, t)
        typ_to_pvar <- typ_to_pvar.Add(t, pvar :: typ_to_pvar.[t, []])

    static member public ContainsVar(pvar:id) = pvar_to_typ.ContainsKey(pvar)
    static member public Type(pvar:id) = pvar_to_typ.[pvar]
    static member public PVars(t:id)   = typ_to_pvar.[t]

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// EXTERNAL REFINEMENT LANGUAGE.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and refn_ext =
    | REUnit                         // Unit         [unit]
    | REBase  of id                  // Base         [nat]
    | RECtor  of id * refn_ext       // Constructor  [S nat]
    | REArrA  of refn_ext * refn_ext // Arrow        [S nat -> unit]
    | REArrE  of refn_ext * refn_ext // Arrow        [S nat => unit]
    | RETup   of refn_ext list       // Tuple        [unit * Nil unit * S nat]
    | RENot   of refn_ext            // Negation     [not(S (S nat)))]
    | REAnd   of refn_ext list       // Intersection [/\(nat -> list, S nat -> Nil unit)]
    | REOr    of refn_ext list       // Union        [\/(nat -> list, S nat -> Nil unit)]
    | REPTop  of id                  // Polymorphic  ['a]

    member public r.Nodes
      with get() =
        match r with
        | REPTop _ -> 1
        | REUnit -> 1
        | REBase _ -> 1
        | RECtor (_, r) -> 1 + r.Nodes
        | REArrA (r1, r2) -> 1 + r1.Nodes + r2.Nodes
        | REArrE (r1, r2) -> 1 + r1.Nodes + r2.Nodes
        | RETup rs | REAnd rs | REOr rs -> 1 + List.sumBy (fun (r:refn_ext) -> r.Nodes) rs
        | RENot r         -> 1 + r.Nodes 

    // Operator precedence.
    member private r.precedence () =
        match r with
        | REUnit | REBase _  | RECtor(_, REUnit) | REPTop _ -> 900
        | RECtor _            -> 850
        | REArrA _ | REArrE _ -> 700
        | REAnd _ | REOr _ | RENot _ | RETup _ -> 0

    // Convert into String.
    override this.ToString() =
        let rec to_string_prec (p : int) (r : refn_ext) =
            let rp = r.precedence()
            let (call, callr) = (to_string_prec rp, to_string_prec (rp - 1))
            let out =
                match r with
                | REUnit            -> "()"
                | REBase b          -> b
                | RECtor(c, REUnit) -> sprintf "%s" c
                | RECtor(c, r)      -> sprintf "%s %s" c (call r)
                | REArrA(r1, r2)    -> sprintf "%s -> %s" (call r1) (callr r2)
                | REArrE(r1, r2)    -> sprintf "%s => %s" (call r1) (callr r2)
                | RETup rs          -> "(" + String.concat ", " (List.map call rs) + ")"
                | RENot r           -> sprintf "not(%s)" (call r)
                | REAnd [r]         -> to_string_prec p r
                | REOr  [r]         -> to_string_prec p r
                | REAnd rs          -> sprintf "/\\(%s)" (String.concat ", " (List.map call rs))
                | REOr  rs          -> sprintf "\\/(%s)" (String.concat ", " (List.map call rs))
                | REPTop s          -> s
            if rp <= p && rp <> 0 then sprintf "(%s)" out else out
        to_string_prec 0 this

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// NORMALIZED REFINEMENT LANGUAGE.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// A refinement.
and rtup     = List<refn>
and rarr     = refn * refn
and ctor_map = Map<id, refn>
and [<Extension>] refn (node:refn_node, tag:int, hkey:int) =
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // PROPERTIES.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    member public  this.Node = node
    member private this.Tag  = tag
    member private this.HKey = hkey

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // CHECKING CONSTRUCTORS.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Does this refinement have a particular constructor?
    member public this.IsUTop with get() = match this.Node with NUTop   -> true | _ -> false
    member public this.IsUBot with get() = match this.Node with NUBot   -> true | _ -> false
    member public this.IsBTop with get() = match this.Node with NBTop _ -> true | _ -> false
    member public this.IsBase with get() = match this.Node with NBase _ -> true | _ -> false
    member public this.IsTup  with get() = match this.Node with NTup  _ -> true | _ -> false
    member public this.IsArr  with get() = match this.Node with NArr  _ -> true | _ -> false
    member public this.IsLib  with get() = match this.Node with NLib  _ -> true | _ -> false
    member public this.IsPoly with get() = match this.Node with NPoly _ -> true | _ -> false
    member public this.IsPTop with get() = match this.Node with NPTop _ -> true | _ -> false

    // Static versions for use in map().
    static member public IsUTopFn(r:refn) = r.IsUTop
    static member public IsUBotFn(r:refn) = r.IsUBot
    static member public IsBTopFn(r:refn) = r.IsBTop
    static member public IsBaseFn(r:refn) = r.IsBase
    static member public IsTupFn (r:refn) = r.IsTup
    static member public IsArrFn (r:refn) = r.IsArr
    static member public IsLibFn (r:refn) = r.IsLib
    static member public IsPTopFn(r:refn) = r.IsPTop
    static member public IsPolyFn(r:refn) = r.IsPoly

    // Assert the constructor and extract its contents.
    static member private Fail() = failwith "Unexpected behavior."
    member public this.BTop with get() = match this.Node with NBTop v -> v | _ -> refn.Fail()
    member public this.Tup  with get() = match this.Node with NTup  v -> v | _ -> refn.Fail()
    member public this.Arr  with get() = match this.Node with NArr  v -> v | _ -> refn.Fail()
    member public this.Base with get() = match this.Node with NBase v -> v | _ -> refn.Fail()
    member public this.Lib  with get() = match this.Node with NLib  v -> v | _ -> refn.Fail()
    member public this.PTop with get() = match this.Node with NPTop v -> v | _ -> refn.Fail()
    member public this.Poly with get() = match this.Node with NPoly v -> v | _ -> refn.Fail()

    // Static versions for use in map().
    static member public BTopFn(r:refn) = r.BTop
    static member public BaseFn(r:refn) = r.Base
    static member public TupFn (r:refn) : rtup list = r.Tup
    static member public ArrFn (r:refn) = r.Arr
    static member public LibFn (r:refn) = r.Lib
    static member public PolyFn(r:refn) = r.Poly

    // Number of AST nodes.
    static member public NodesFn(r:refn) = r.Nodes
    member public this.Nodes
      with get() =
        match this.Node with
        | NUTop | NUBot | NLib _ | NBTop _ | NPTop _ -> 1
        | NBase m -> 1 + Map.fold (fun acc _ (v:refn) -> acc + v.Nodes) m.Count m
        | NTup rs -> let tup_nodes (rt:rtup) = List.fold (fun acc (r:refn) -> acc + r.Nodes) 1 rt
                     List.fold (fun acc (rt:rtup) -> acc + tup_nodes rt) 1 rs
        | NArr cnf -> let arr_nodes ((r1,r2):rarr) = 1 + r1.Nodes + r2.Nodes
                      let arr_or_nodes (rs:rarr list) =
                          List.fold (fun acc r -> acc + arr_nodes r) 1 rs
                      List.fold (fun acc rs -> acc + arr_or_nodes rs) 1 cnf.AsList
        | NPoly ps -> 1 + ps.Count

    // Is a singleton.
    static member public IsSingletonFn(r:refn) = r.IsSingleton
    member public this.IsSingleton
      with get() =
        match this.Node with
        | NPoly ps  -> ps.Count = 1
        | NPTop _   -> false
        | NUTop     -> true
        | NUBot     -> false
        | NLib x    -> true
        | NBTop s   -> false
        | NArr _    -> false
        | NTup rs   -> rs.IsSingleton && List.forall refn.IsSingletonFn rs.Singleton
        | NBase rs  -> rs.IsSingleton && (snd rs.Singleton).IsSingleton

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // CONVERT TO OTHER FORMS.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Convert into a refn_ext
    static member public ExternalFn(r:refn) = r.External
    member public this.External
      with get() =
        match this.Node with
        | NPoly ps  -> List.map REBase (Set.toList ps) |> REOr
        | NPTop p   -> REPTop p
        | NLib x    -> REBase x
        | NUBot     -> REOr []
        | NUTop     -> REUnit
        | NBTop b   -> REBase b
        | NBase rs  -> REOr(rs.ToList().map(fun (c, r) -> RECtor(c, r.External)))
        | NTup rss  -> REOr(rss.map(List.map refn.ExternalFn >> RETup))
        | NArr  cnf ->
            let orFn  ls = ls |> List.map (fun (a:refn, b:refn) -> (a.External, b.External))
                              |> List.map REArrA |> REOr
            cnf.FoldList(orFn, REAnd)

    // Print as string.
    override this.ToString() = this.External.ToString()

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // TRANSFORM.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Unfold a base top refinement.
    member public this.Unfold(t_vars:typ list) : refn =
        HC.nbase (Ctors.AllCtors(this.BTop, t_vars).map(fun (c:ctor) -> (c.name, c.t.Top)).ToMap())
    member public this.UnfoldT(t:typ) : refn =
        this.Unfold(snd t.Base)

    // Breaks the list of refinements underlying a tuple refinement into a list of list of
    // refinements: [(a, b, c); (d, e, f)] ==> [[a; d]; [b; e]; [c; f]]
    member public this.TupCols with get() : refn list list = this.Tup.transpose()

    [<Extension>]
    static member PullOr(r:refn) : refn seq =
        match r.Node with
        | NUTop | NUBot | NBTop _ | NPTop _ -> Seq.singleton r
        | NBase m -> let map_fn (c,r:refn) =
                       (refn.PullOr r).map(fun r -> HC.nbase (Map.empty.Add(c, r)))
                     Seq.ofList (m.ToList()) |> Seq.map map_fn |> Seq.concat
        | NTup ts -> let map_fn (rs:rtup) =
                         Seq2.cartesianList(rs.map(refn.PullOr)) |>
                         Seq.map (fun rs -> HC.ntup [rs])
                     Seq.ofList ts |> Seq.map map_fn |> Seq.concat
        | NArr cnf -> let dnf = cnf.DNF()
                      let handle_arr (r1:refn, r2:refn) =
                          refn.PullOr(r1).map(fun r -> (r, r2))
                      let handle_and (rs:rarr list) =
                             Seq.ofList rs |> Seq.map handle_arr |> Seq.concat
                             |> Seq.map (fun r -> [r]) |> Seq.toList |> CNF.Create
                             |> HC.narr
                      dnf.AsList.map(handle_and) |> Seq.ofList
        | NPoly ps -> ps |> Set.toSeq |> Seq.map (fun x -> HC.npoly (Set.singleton x))
        | _ -> Seq.empty
                      

    [<Extension>]
    // Convert the disjunction of a list of refinements rs at type t into a single refinement
    // using logical transformations.
    static member inline PushOr(rs:refn list, t:typ) : refn =
        let rec push_or (rs:refn list) (t:typ) : refn =
            match t.Node with
            | TPoly p -> if List.exists refn.IsPTopFn rs then HC.nptop p
                         else HC.npoly (Set.unionMany (rs.map(refn.PolyFn)))
            | TUnit   -> if rs.unfilter(refn.IsUBotFn).IsEmpty then HC.nubot else HC.nutop
            | TTup  _ -> HC.ntup (List.collect refn.TupFn rs)
            | TArr _  -> let dnf = DNF.Create(rs.map(refn.ArrFn >> (fun cnf -> cnf.AsList)))
                         HC.narr (dnf.CNF().FoldList(List.concat, CNF.Create))
            | TBase(b, vs) ->
                         if List.exists refn.IsBTopFn rs then HC.nbtop b
                         else let ctors = List.collect Map.toList (rs.map(refn.BaseFn))
                              HC.nbase (ctors.ToMultiMap().map(fun c rs -> push_or rs (Ctors.Type c vs)))
        push_or rs t

    [<Extension>]
    // Convert the disjunction of a list of refinements rs at type t into a single refinement
    // using logical transformations.
    // Applies sound but incomplete identities on tuple and function disjunction to eliminate
    // all disjunction from the entire system.
    static member inline PushOrUnsound(rs:refn list, t:typ) : refn =
        let rec push_or (rs:refn list) (t:typ) : refn =
            match t.Node with
            | TPoly _ | TUnit _  -> refn.PushOr(rs, t)
            | TTup ts   -> HC.ntup [List.map2 push_or (rs.collect(refn.TupFn).transpose()) ts]
            | TArr(s,t) -> let dnf = DNF.Create(rs.map(refn.ArrFn >> (fun cnf -> cnf.AsList)))
                           let push_fn lss =
                               let (rs1, rs2) = List.unzip (List.concat lss)
                               [(push_or rs1 s, push_or rs2 t)]
                           HC.narr (dnf.CNF().FoldList(push_fn, CNF.Create))
            | TBase(b, vs) ->
                         if List.exists refn.IsBTopFn rs then HC.nbtop b
                         else let ctors = List.collect Map.toList (rs.map(refn.BaseFn))
                              HC.nbase (ctors.ToMultiMap().map(fun c rs -> push_or rs (Ctors.Type c vs)))
        push_or rs t

    [<Extension>]
    // Convert the conjunction of a list of refinements rs at type t into a single refinement
    // using logical transformations.
    static member PushAnd(rs:refn list, t:typ) : refn =
        let rec push_and (rs:refn list) (t:typ) : refn =
            match t.Node with
            | TPoly p -> let dnf = CNF.Create(rs.unfilter(refn.IsPTopFn)
                                                .map(refn.PolyFn >> Set.toList)).DNF().AsSet
                         if dnf.IsEmpty || Set.exists (fun (s:Set<_>) -> s.Count > 1) dnf then t.Bot
                         elif Set.exists Set.isEmpty dnf then t.Top
                         else Set.map (Set.toList >> List.head) dnf |> HC.npoly
            | TUnit   -> if List.exists refn.IsUBotFn rs then HC.nubot else HC.nutop
            | TTup ts -> let dnf = CNF.Create(rs.map(refn.TupFn)).DNF()
                         let map_fn (rows:rtup list) = List.map2 push_and (HC.ntup rows).TupCols ts
                         HC.ntup (dnf.AsList.map(map_fn))
            | TArr _  -> HC.narr (CNF.JoinAnd(rs.map(refn.ArrFn)))
            | TBase(b, vs) ->
                // Filter "top" refinements.
                let rs' = rs.filter(refn.IsBaseFn).map(refn.BaseFn >> Map.toList)

                // If there are no refinements left, then we are top.
                if rs'.IsEmpty then t.Top
                else
                    // Push the "and" through the "ors" of ctor refinements.
                    let dnf = CNF.Create(rs').DNF()

                    // Joins an "and" of constructors.
                    let join_and (cs : (id * refn) list) : Choice<bool, ctor_map> =
                        let cs = cs.ToMultiMap()
                        if          cs.IsEmpty      then Choice1Of2(true)
                        else if not(cs.IsSingleton) then Choice1Of2(false)
                        else Choice2Of2(cs.map(fun c rs -> push_and rs (Ctors.Type c vs)))

                    // Join each of the packets of "ands."
                    let joined = dnf.FoldList(join_and, ident)

                    // If any of the packets are top then the whole thing is top.
                    if List.exists (function Choice1Of2 true -> true | _ -> false) joined then t.Top
                    else List.choose (function Choice2Of2 s -> Some s | _ -> None) joined
                         |> (fun rs -> refn.PushOr(rs.map(HC.nbase), t))
        push_and rs t

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // SUBSUMPTION.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Subrefinement.
    static member private subrefines_cache =
       new System.Collections.Generic.Dictionary<refn*refn*typ, bool>()
    member public this.SubRefines(that:refn, t:typ) : bool =
        let sub  (a:refn)  (b:refn) (t:typ)  = a.SubRefines(b, t)
        let mutable out = false
        if refn.subrefines_cache.TryGetValue((this,that,t), &out) then out
        else
          out <-
            match this.Node, that.Node, t.Node with
            | _, NUTop, TUnit | NUBot, _, TUnit -> true
            | NPTop p1,  NPTop p2, TPoly p  -> p1 = p2 && p2 = p
            | NPoly ps,  NPTop p1, TPoly p  -> p = p1
            | NPoly ps1, NPoly ps2, TPoly p -> Set.forall (fun p1 -> ps2.Contains(p1)) ps1
            | NBTop b1,  NBTop b2, TBase(bt, _) -> b1.Equals(b2) && b2.Equals(bt)
            | NBase rs1, NBTop b,  TBase(bt, vs) ->
                b.Equals(bt) && Map.forall (fun (c:id) _ -> (Ctors.GetCtor c vs).datatype.Equals(b)) rs1
            | NBTop b, NBase rs2,  TBase(bt, vs) -> sub (this.Unfold vs) that t
            | NBase rs1, NBase rs2, TBase(bt, vs) ->
                let forall_fn c1 r1 = rs2.ContainsKey(c1) && sub r1 rs2.[c1] (Ctors.Type c1 vs)
                Map.forall (fun (c:id) _ -> (Ctors.GetCtor c vs).datatype.Equals(bt)) rs1 &&
                Map.forall (fun (c:id) _ -> (Ctors.GetCtor c vs).datatype.Equals(bt)) rs2 &&
                Map.forall forall_fn rs1
            | NTup lss1, NTup lss2, TTup ts ->
                let forall_fn ls1 =
                    let exists_fn ls2 = List.forall ident (List.map3 sub ls1 ls2 ts)
                    List.exists exists_fn lss2
                List.forall forall_fn lss1
            | NArr  cnf1, NArr  cnf2, TArr(t1, t2) ->
                // Subtyping between ors of arrows.
                let sub_arr_or (ls1:list<refn * refn>) (ls2:list<refn * refn>) =
                    let sub_arr (a1:refn, b1:refn) (a2:refn, b2:refn) =
                        List.forall (fun x -> x) [sub a2 a1 t1; sub b1 b2 t2]
                    List.forall (fun l1 -> List.exists (sub_arr l1) ls2) ls1

                // Subtyping the outer disjunction.
                List.forall (fun ls2 -> List.exists (fun ls1 -> sub_arr_or ls1 ls2) cnf1.AsList) cnf2.AsList
            | _, _, _ -> false
          refn.subrefines_cache.[(this,that,t)] <- out
          out


    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // EQUALITY AND COMPARISON.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    override this.Equals(thatobj) =
        match thatobj with
        | :? refn as that -> this.Tag = that.Tag
        | _ -> false

    override this.GetHashCode() = this.HKey

    interface System.IComparable with
       member this.CompareTo thatobj =
         match thatobj with
         | :? refn as that -> compare this.Tag that.Tag
         | _ -> invalidArg "thatobj" "cannot comapre values of different types"
// The internal refn_node type.
and [<NoComparison;StructuralEquality;Extension>] refn_node = 
    | NUBot                      // Unit bottom.
    | NUTop                      // Unit top.
    | NBTop of id                // Base top.
    | NBase of ctor_map          // Disjunction of constructor refinements.
    | NArr  of CNF<rarr>         // CNF of arrow refinements.
    | NTup  of List<rtup>        // Disjunction of tuple refinements.
    | NLib  of id                // The name of a value from the library.
    | NPTop of id                // Top for a polymorphic type
    | NPoly of Set<id>           // Disjunction of polymorphic variables.

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// TYPE LANGUAGE.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and typ (node:typ_node, tag:int, hkey:int) =
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // PROPERTIES.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    member public  this.Node = node
    member private this.Tag  = tag
    member private this.HKey = hkey
    static member public NodeFn(r:refn) = r.Node

    // Convert into nrefn.
    member public this.Top
      with get() =
        match this.Node with
        | TPoly p      -> HC.nptop p
        | TUnit        -> HC.nutop
        | TBase(b, _)  -> HC.nbtop b
        | TTup ts      -> HC.ntup [List.map (fun (t:typ) -> t.Top) ts]
        | TArr(t1, t2) -> HC.narr (CNF.Create1(t1.Top, t2.Top))

    // Convert into bottom.
    member public this.Bot
      with get() =
        match this.Node with
        | TPoly p      -> HC.npoly Set.empty
        | TUnit        -> HC.nubot
        | TBase(b, _)  -> HC.nbase Map.empty
        | TTup ts      -> HC.ntup [List.map (fun (t:typ) -> t.Bot) ts]
        | TArr(t1, t2) -> HC.narr (CNF.Create([[]]))

    // Substitute polymorphic types for other types.
    member public this.Subst(m:Map<id, typ>) : typ =
        match this.Node with
        | TPoly p -> if m.ContainsKey(p) then m.[p] else this
        | TUnit -> this
        | TBase(x, vs) -> HC.tbase(x, vs.map(fun t -> t.Subst(m)))
        | TTup ts -> HC.ttup (ts.map(fun t -> t.Subst(m)))
        | TArr(t1, t2) -> HC.tarr(t1.Subst(m), t2.Subst(m))

    // Operator precedence.
    member private t.precedence () =
        match t.Node with
        | TUnit | TBase(_, []) | TPoly _ -> 900
        | TBase _                   -> 850
        | TTup _                    -> 800
        | TArr _                    -> 700

    // Convert into String.
    override this.ToString() =
        let rec to_string_prec (p : int) (t : typ) =
            let rp = t.precedence()
            let (call, callr) = (to_string_prec rp, to_string_prec (rp - 1))
            let out =
                match t.Node with
                | TUnit             -> "unit"
                | TBase (b,ls)      ->
                    if ls.IsEmpty then b
                    elif ls.IsSingleton then sprintf "%O %s" ls.Singleton b
                    else
                       let to_tup = "(" + String.Join(", ", ls.map(fun l -> l.ToString())) + ")"
                       sprintf "%s %s" to_tup b
                | TPoly b           -> b
                | TArr(t1, t2)      -> sprintf "%s -> %s" (call t1) (callr t2)
                | TTup ts           -> String.concat " * " (List.map call ts)
            if rp <= p && rp <> 0 then sprintf "(%s)" out else out
        to_string_prec 0 this

    // Does this type have a particular constructor?
    member public this.IsBase = match this.Node with TBase _ -> true | _ -> false
    member public this.IsArr  = match this.Node with TArr  _ -> true | _ -> false
    member public this.IsTup  = match this.Node with TTup  _ -> true | _ -> false
    member public this.IsUnit = match this.Node with TUnit _ -> true | _ -> false
    member public this.IsPoly = match this.Node with TPoly _ -> true | _ -> false

    // Assert the constructor and extract the contents of a type.
    static member private Fail() = failwith "Unexpected behavior."
    member public this.Arr  with get() = match this.Node with TArr(a, b) -> (a, b) | _ -> typ.Fail()
    member public this.Tup  with get() = match this.Node with TTup ts    -> ts     | _ -> typ.Fail()
    member public this.Base with get() = match this.Node with TBase(a,b) -> (a, b) | _ -> typ.Fail()
    member public this.Poly with get() = match this.Node with TPoly b    -> b      | _ -> typ.Fail()
    
    // The root of names of variables of a particular type.
    member public this.Name with get() = match this.Node with
                                         | TUnit   -> "u"
                                         | TArr  _ -> "f"
                                         | TTup  _ -> "t"
                                         | TBase(b,_) -> b.[0].ToString()
                                         | TPoly _ -> "p"

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // EQUALITY AND COMPARISON.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    override this.Equals(thatobj) =
        match thatobj with
        | :? typ as that -> this.Tag = that.Tag
        | _ -> false

    override this.GetHashCode() = this.HKey

    interface System.IComparable with
       member this.CompareTo thatobj =
         match thatobj with
         | :? typ as that -> compare this.Tag that.Tag
         | _ -> invalidArg "thatobj" "cannot compare values of different types"

and [<StructuralEquality;NoComparison>] typ_node =
    | TBase of id * typ list // Polymorphic type  [nat] (Type variables are in order in the list)
    | TArr  of typ * typ     // Arrow type [nat -> list]
    | TTup  of typ list      // Tuple type [nat * list * bool]
    | TUnit                  // Unit type  [unit]
    | TPoly of id            // Polymorphic type ['a]

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// EXPRESSION LANGUAGE.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and lambda   = { argl : id; arg_type : typ; body : expr }
and fixpoint = { argf : id; arg_type : typ; body : expr; name : id; ret_type : typ }
and expr (node:expr_node, tag:int, hkey:int) =
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // PROPERTIES.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    member public  this.Node = node
    member private this.Tag  = tag
    member private this.HKey = hkey
    static member public NodeFn(r:refn) = r.Node

    // The number of AST nodes in an expression.
    member public this.Nodes =
        match this.Node with
        | EHole _        -> 0
        | EUnit | EVar _ -> 1
        | EApp(e1, e2)   -> 1 + e1.Nodes + e2.Nodes
        | EFun lam       -> 1 + lam.body.Nodes
        | EFix fix       -> 1 + fix.body.Nodes
        | ECtor(_, e)    -> 1 + e.Nodes
        | EMatch(e, bs)  -> 1 + e.Nodes + List.fold (fun b (_, _, e:expr) -> b + e.Nodes) 0 bs
        | ETup es        -> List.fold (+) 0 (es.map(fun e -> e.Nodes))
        | EProj(_, e)    -> 1 + e.Nodes

    // The size of an expression.
    member public this.Size =
        match this.Node with
        | EHole _        -> 1
        | EUnit | EVar _ -> 1
        | EApp(e1, e2)   -> 1 + Math.Max(e1.Size, e2.Size)
        | EFun lam       -> 1 + lam.body.Size
        | EFix fix       -> 1 + fix.body.Size
        | ECtor(_, e)    -> 1 + e.Size
        | EMatch(e, bs)  -> let b = bs.map(fun (_, _, e:expr) -> e.Size) |> List.max
                            1 + Math.Max(e.Size, b)
        | ETup es        -> 1 + (es.map(fun e -> e.Size) |> List.max)
        | EProj(i, e)    -> 1 + e.Size

    // Fill holes given a mapping from holes to expressions.
    member public this.FillHoles(m : Map<hole, expr>) =
        match this.Node with
        | EHole h -> if m.ContainsKey(h) then m.[h] else this
        | EUnit | EVar _ -> this
        | EApp(e1, e2)   -> HC.eapp(e1.FillHoles(m), e2.FillHoles(m))
        | EFun lam       -> HC.efun {lam with body = lam.body.FillHoles(m)}
        | EFix fix       -> HC.efix {fix with body = fix.body.FillHoles(m)}
        | ECtor(c, e)    -> HC.ector(c, e.FillHoles(m))
        | EMatch(e, bs)  -> HC.ematch(e.FillHoles(m),
                                      List.map (fun (c, x, e:expr) -> (c, x, e.FillHoles(m))) bs)
        | ETup es        -> HC.etup(List.map (fun (e:expr) -> e.FillHoles(m)) es)
        | EProj(i, e)    -> HC.eproj(i, e.FillHoles(m))

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // TOSTRING.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Operator precedence for refinements.
    member private this.precedence () =
        match this.Node with
        | EUnit | EVar _ | EHole _ -> 900
        | ECtor(c, e) -> match e.Node with EUnit -> 900 | _ -> 500
        | EApp _ | EProj _ -> 500
        | EFun _ | EFix _  -> 200
        | EMatch _ -> 150
        | ETup _ -> 0

    // Convert expression to String.
    static member private to_string_prec (p : int) (n : int) (e : expr) =
        let ep = e.precedence()
        let ind  = String.replicate n       "  "
        let ind1 = String.replicate (n + 1) "  "
        let (call, callr, call0) = (expr.to_string_prec ep, expr.to_string_prec (ep - 1),
                                    expr.to_string_prec 0)
        let branch (c, x, e) = sprintf "%s| %s %s -> %s" ind c x (callr (n+1) e)
        let out =
            match e.Node with
            | EHole h         -> sprintf "h%d" h
            | EUnit           -> "()"
            | EVar v          -> v
            | EApp(e1, e2)    -> sprintf "%s %s"  (call n e1) (call n e2)
            | ECtor(c, e)     -> match e.Node with EUnit -> sprintf "%s" c
                                                   | _   -> sprintf "%s %s"  c (call n e)
            | EProj(i, e)     -> sprintf "#%d %s" i (call n e)
            | EFun f          -> sprintf "fun (%s:%O) ->\n%s%s"
                                          f.argl f.arg_type ind1 (call0 (n + 1) f.body)
            | EFix f          -> sprintf "fix %s (%s:%O) : %O ->\n%s%s"
                                          f.name f.argf f.arg_type f.ret_type ind1
                                          (call0 (n + 1) f.body)
            | ETup es         -> sprintf "(%s)" (String.concat ", " (List.map (call n) es))
            | EMatch(e, bs)   -> sprintf "match %s with\n%s" (call n e)
                                        (String.concat "\n" (List.map branch bs))
        if ep <= p && ep <> 0 then "(" + out + ")" else out
    member public this.ToString(n:int) = expr.to_string_prec 0 n this   
    override      this.ToString()      = expr.to_string_prec 0 0 this

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // EQUALITY AND COMPARISON.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    override this.Equals(thatobj) =
        match thatobj with
        | :? expr as that -> this.Tag = that.Tag
        | _ -> false

    override this.GetHashCode() = this.HKey

    interface System.IComparable with
       member this.CompareTo thatobj =
         match thatobj with
         | :? expr as that -> compare this.Tag that.Tag
         | _ -> invalidArg "thatobj" "cannot comapre values of different types"

and [<StructuralEquality;NoComparison>] expr_node =
    | EHole  of hole                          // A hole           [No syntax]
    | EUnit                                   // Unit value       [()]
    | EVar   of id                            // Variable         [x]
    | EApp   of expr * expr                   // Application      [e1 e2]
    | EFun   of lambda                        // Function         [\x:t1.e]
    | EFix   of fixpoint                      // Fixpoint         [\f(x:t1):t2.e]
    | ECtor  of id * expr                     // Constructor      [ctor, var]
    | EMatch of expr * (id * id * expr) list  // Match statement  [scrutinee, (ctor, var, branch)]
    | ETup   of expr list                     // Tuple value      [(1, 2, 3)]
    | EProj  of int * expr                    // Tuple projection [#1 (1, 2)]

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Value Language.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and env = Map<id, value>
and gam = Map<id, typ>
and vfun = {argv:id; arg_type:typ; body:expr; env:env; self:id option; gam:gam}
and value (node:value_node, tag:int, hkey:int) =
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // PROPERTIES.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    member public  this.Node = node
    member private this.Tag  = tag
    member private this.HKey = hkey
    static member public NodeFn(r:refn) = r.Node

    member public this.TryRefn
      with get() =
        match this.Node with
        | VUnit       -> Some HC.nutop
        | VCtor(c, v) -> match v.TryRefn with
                         | None -> None
                         | Some r -> Some (HC.nbase (Map.empty.Add(c, r)))
        | VTup vs     -> let rs = List.map (fun (v:value) -> v.TryRefn) vs
                         if List.exists Option.isNone rs then None
                         else
                            Some (HC.ntup[rs.map(Option.get)])
        | VRefn r     -> Some r
        | VFun _      -> None

    member public this.Expr
      with get() =
        match this.Node with
        | VUnit       -> HC.eunit
        | VCtor(c, v) -> HC.ector(c, v.Expr)
        | VTup vs     -> HC.etup(List.map (fun (v:value) -> v.Expr) vs)
        | VFun vf     -> HC.efun {argl=vf.argv; arg_type=vf.arg_type; body=vf.body}
        | VRefn _     -> failwith "You can't just convert a refinement into an expression,
                                   silly - that's what synthesis is!"

    override this.ToString() = this.Expr.ToString()

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // EQUALITY AND COMPARISON.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    override this.Equals(thatobj) =
        match thatobj with
        | :? value as that -> this.Tag = that.Tag
        | _ -> false

    override this.GetHashCode() = this.HKey

    interface System.IComparable with
       member this.CompareTo thatobj =
         match thatobj with
         | :? value as that -> compare this.Tag that.Tag
         | _ -> invalidArg "thatobj" "cannot compare values of different types"
and [<StructuralEquality;NoComparison>] value_node =
    | VUnit                                   // Unit value       [()]
    | VFun   of vfun                          // Function         [\x:t1.e]
    | VCtor  of id * value                    // Constructor      [ctor, var]
    | VTup   of value list                    // Tuple value      [(1, 2, 3)]
    | VRefn  of refn                          // Refinement value [refn]

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// SYNTHESIS PROBLEM.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Declarations.
and decl =
    | DDatatype of id * (id list) * (id * typ) list // A type name and a list of variables and
                                                    // constructors.
    | DLibrary  of id * expr            // An expression declared prior to the synth problem.

    override d.ToString () =
        let print_ctor(c, t:typ) =
            match t.Node with
            | TUnit -> sprintf "| %s\n" c
            | _     -> sprintf "| %s of %O\n" c t
        match d with
        | DDatatype (t, vs, cs) ->
            let ts = vs.map(HC.tpoly)
            sprintf "type %O =\n%s" (HC.tbase(t, ts)) (String.concat "" (List.map print_ctor cs))
        | DLibrary  (x, e)  -> sprintf "let  %s =\n%O" x e

// Synthesis problem.
and synth_problem = {
    declarations : decl list
    synth_name   : id
    synth_type   : typ
    synth_refn   : refn_ext option
    is_rec       : bool
}
    with
    override s.ToString () =
        let decls =
             s.declarations
          |> List.map (fun d -> d.ToString())
          |> String.concat "\n"
        let r = if s.is_rec then " rec " else ""
        sprintf "%s\nlet %s%O : %O |>\n %O = ?" decls r s.synth_name s.synth_type s.synth_refn.Value

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// HASHCONS INFRASTRUCTURE.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and HC private() =
    // Tables.
    static let refn_table = new System.Collections.Generic.Dictionary<refn_node, refn>()
    static let mutable fresh_tag_r = 0
    static let expr_table = new System.Collections.Generic.Dictionary<expr_node, expr>()
    static let mutable fresh_tag_e = 0
    static let typ_table = new System.Collections.Generic.Dictionary<typ_node, typ>()
    static let mutable fresh_tag_t = 0
    static let value_table = new System.Collections.Generic.Dictionary<value_node, value>()
    static let mutable fresh_tag_v = 0

    // Refinements.
    static member private hashcons_refn(r:refn_node) : refn =
        let mutable out = Unchecked.defaultof<refn>
        if refn_table.TryGetValue(r, &out) then out
        else
            let hcons = refn(r, fresh_tag_r, hash r)
            fresh_tag_r <- fresh_tag_r + 1
            refn_table.Add(r, hcons)
            hcons
    static member public nubot   : refn = HC.hashcons_refn(NUBot)
    static member public nutop   : refn = HC.hashcons_refn(NUTop)
    static member public nbtop v : refn = HC.hashcons_refn(NBTop v)
    static member public nbase v : refn = HC.hashcons_refn(NBase v)
    static member public narr  v : refn = HC.hashcons_refn(NArr v)
    static member public ntup  v : refn = HC.hashcons_refn(NTup v)
    static member public nlib  v : refn = HC.hashcons_refn(NLib v)
    static member public npoly v : refn = HC.hashcons_refn(NPoly v)
    static member public nptop v : refn = HC.hashcons_refn(NPTop v)

    // Expressions.
    static member private hashcons_expr(e:expr_node) : expr =
        let mutable out = Unchecked.defaultof<expr>
        if expr_table.TryGetValue(e, &out) then out
        else
            let hcons = expr(e, fresh_tag_e, hash e)
            fresh_tag_e <- fresh_tag_e + 1
            expr_table.Add(e, hcons)
            hcons
    static member public eunit    : expr = HC.hashcons_expr(EUnit)
    static member public ehole  v : expr = HC.hashcons_expr(EHole v)
    static member public evar   v : expr = HC.hashcons_expr(EVar v)
    static member public eapp   v : expr = HC.hashcons_expr(EApp v)
    static member public efix   v : expr = HC.hashcons_expr(EFix v)
    static member public efun   v : expr = HC.hashcons_expr(EFun v)
    static member public ector  v : expr = HC.hashcons_expr(ECtor v)
    static member public ematch v : expr = HC.hashcons_expr(EMatch v)
    static member public etup   v : expr = HC.hashcons_expr(ETup v)
    static member public eproj  v : expr = HC.hashcons_expr(EProj v)

    // Types.
    static member private hashcons_typ(t:typ_node) : typ =
        let mutable out = Unchecked.defaultof<typ>
        if typ_table.TryGetValue(t, &out) then out
        else
            let hcons = typ(t, fresh_tag_t, hash t)
            fresh_tag_t <- fresh_tag_t + 1
            typ_table.Add(t, hcons)
            hcons
    static member public tarr  v : typ = HC.hashcons_typ(TArr v)
    static member public tbase (v:id * typ list) : typ = HC.hashcons_typ(TBase v)
    static member public ttup  v : typ = HC.hashcons_typ(TTup v)
    static member public tunit   : typ = HC.hashcons_typ(TUnit)
    static member public tpoly v : typ = HC.hashcons_typ(TPoly v)

    // Values.
    static member private hashcons_value(v:value_node) : value =
        let mutable out = Unchecked.defaultof<value>
        if value_table.TryGetValue(v, &out) then out
        else
            let hcons = value(v, fresh_tag_v, hash v)
            fresh_tag_v <- fresh_tag_v + 1
            value_table.Add(v, hcons)
            hcons
    static member public vfun  v : value = HC.hashcons_value(VFun v)
    static member public vtup  v : value = HC.hashcons_value(VTup v)
    static member public vrefn v : value = HC.hashcons_value(VRefn v)
    static member public vctor v : value = HC.hashcons_value(VCtor v)
    static member public vunit   : value = HC.hashcons_value(VUnit)

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// EXTENSIONS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[<Extension>]
type LangExtensions private () =
    [<Extension>]
    static member inline Fresh(g:gam, t:typ) =
        let rec fresh_name s n =
            let name = sprintf "%s%d" s n
            if g.ContainsKey(name) then fresh_name s (n + 1)
            else name
        fresh_name t.Name 1

    [<Extension>]
    static member inline ToInfoT(this:id_info) : id_info_t =
        {name=this.name; t=this.t; about=this.about}

    [<Extension>]
    static member inline ToInfo(this:id_info_t, rs:refns) : id_info =
        {name=this.name; t=this.t; about=this.about; rs=rs}


    [<Extension>]
    static member inline ToGam(this:id_info_t list) : gam =
        this.map(fun x -> (x.name, x.t)).ToMap()

    [<Extension>]
    static member inline ToGam(this:id_info list) : gam =
        this.map(fun x -> (x.name, x.t)).ToMap()

    [<Extension>]
    static member inline StructuralCheck(this:id_info_t, mustBeDecIn:id option) =
       mustBeDecIn.IsNone || (this.about.DecIn.IsSome && this.about.DecIn.Value = mustBeDecIn.Value)
       || (this.name = mustBeDecIn.Value)

    [<Extension>]
    static member inline StructuralCheck(this:id_info, mustBeDecIn:id option) =
       LangExtensions.StructuralCheck(LangExtensions.ToInfoT(this), mustBeDecIn)