module Synth.Typecheck
open Lang
open Util
open System.Collections.Generic

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// PRELIMINARIES.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// The output of the typechecking process.
type private output = OSuccess | OFailure of string

// Utility functions.
let private join o1 o2 =
    match o1, o2 with
    | OSuccess, OSuccess -> OSuccess
    | OFailure s1, OFailure s2 -> OFailure (s1 + "\n" + s2)
    | OFailure s, _ | _, OFailure s -> OFailure s
let private joins os = List.fold join OSuccess os

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// TYPECHECK DECLARATIONS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Check that a type declaration is properly formed.  Assumes that the type name
// has not yet been added to sigma.
let private type_decl (datatype : id) (vars:id list) (ctors : (id * typ) list) : output =
    let rec ctor_type_exists(t:typ) =
        match t.Node with
        | TPoly p -> List.exists ((=) p) vars
        | TUnit -> true
        | TArr (t1, t2) -> (ctor_type_exists t1) && (ctor_type_exists t2)
        | TTup ts -> List.fold (fun b t -> b && ctor_type_exists t) true ts
        | TBase(s, ts) -> (s.Equals(datatype) || Ctors.HasType s) &&
                          List.forall ctor_type_exists ts &&
                          (not(s.Equals(datatype)) || ts.Length = vars.Length)

    if Ctors.HasType datatype then OFailure (sprintf "Datatype %s already exists" datatype)
    else joins (List.map (fun (s, t) ->
                   if ctor_type_exists t then OSuccess
                   else OFailure (sprintf "Constructor %s references nonexistent type" s))
                  ctors)

// Check that a refinement matches a type.  The refinement must be free of
// existential arrows.
let rec private refinement (gam:Map<id, typ>) (r : refn_ext) (t : typ) : output =
    let nomatch =
        OFailure (sprintf "Refinement %O does not match type %O" r t)

    match r, t.Node with
    | REPTop p1, TPoly pt -> if pt = p1 then OSuccess else nomatch
    | REUnit,   TUnit   -> OSuccess
    | REBase s, _       ->  if Ctors.HasType(s) && t.IsBase && fst t.Base = s then
                                OSuccess
                            else if Library.Gam().ContainsKey(s) && Library.Gam().[s] = t then
                                OSuccess
                            else if t.IsPoly then
                              if PolyVars.ContainsVar(s) then
                                  if PolyVars.Type(s) = t.Poly then OSuccess else nomatch
                              else
                                  PolyVars.Add(s, t.Poly)
                                  OSuccess
                            else nomatch
    | RECtor (i, r'), TBase(s, vs) ->
        let c = Ctors.GetCtor i vs
        if c.datatype.Equals(s) then refinement gam r' c.t
        else nomatch
    | REArrA (r1, r2), TArr (t1, t2) -> join (refinement gam r1 t1) (refinement gam r2 t2)
    | RETup rs, TTup ts -> joins (List.map2 (refinement gam) rs ts)
    | RENot r', _ -> refinement gam r' t
    | REAnd rs, _ | REOr rs, _-> joins (List.map (fun r -> refinement gam r t) rs)
    | _, _ -> nomatch

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// TYPE INFERENCE ON EXPRESSIONS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let private infer_cache = new Dictionary<gam*expr, typ>()
let rec private infer (gam:gam) (e:expr) : typ =
    let mutable out = Unchecked.defaultof<typ>
    if infer_cache.TryGetValue((gam, e), &out) then out
    else
      out <-
        match e.Node with
        | EHole _       -> failwith "Cannot infer type of hole"
        | EUnit         -> HC.tunit
        | EVar x        -> if gam.ContainsKey(x) then gam.[x]
                           else failwith (sprintf "Variable not in scope: %s" x)
        | ETup es       -> HC.ttup (es.map(infer gam))
        | EProj(i, e)   -> match (infer gam e).Node with
                           | TTup ts -> ts.[i-1]
                           | _ -> failwith "Projection on non-tuple"
        | EFun lam      -> HC.tarr(lam.arg_type, infer (gam.Add(lam.argl, lam.arg_type)) lam.body)
        | EFix fix      -> let gam = gam.Add(fix.argf, fix.arg_type)
                                        .Add(fix.name, HC.tarr(fix.arg_type, fix.ret_type))
                           let t = infer gam fix.body
                           if t <> fix.ret_type then failwith "Bad fixpoint return type"
                           HC.tarr(fix.arg_type, fix.ret_type)
        | EApp(e1, e2)  -> let (tarr, t1) = (infer gam e1, infer gam e2)
                           if not(tarr.IsArr) then failwith "Application of non-function."
                           let (t1', t2) = tarr.Arr
                           if t1 <> t1' then failwith "Application argument and function do not match"
                           else t2
        | ECtor(c, e)   -> let t = infer gam e
                           // WILL FAIL IF THE CONSTRUCTOR IS POLYMORPHIC!!!!!
                           // CHANGE TYPE INFERENCE TO TYPECHECKING
                           HC.tbase ((Ctors.GetCtor c []).datatype, [])
        | EMatch(e, bs) -> let e_typ = infer gam e
                           if not(e_typ.IsBase) then failwith "Scrutinee of non-base type"
                           let (b, vs) = e_typ.Base

                           let tc_branch (c, x, e) = infer (gam.Add(x, Ctors.Type c vs)) e
                           let bs_typ = bs.map(tc_branch)
                           if bs.IsEmpty then failwith "Match statement has no branches."
                           if not(List.forall (fun t -> t = bs_typ.Head) bs_typ) then
                               failwith "Branches do not agree on type"
                           
                           let tc_scrut (c, _, _) = (Ctors.GetCtor c vs).datatype = fst e_typ.Base
                           if not(List.forall tc_scrut bs) then
                               failwith "Branches do not match scrutinee"

                           bs_typ.Head
      infer_cache.[(gam, e)] <- out
      out
                       
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// BIDIRECTIONAL TYPECHECKING OF EXPRESSIONS WITH RESPECT TO REFINEMENTS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let private refn_synth_cache = new Dictionary<Map<id, refn * typ> * expr, (refn * typ) option>()
let private refn_check_cache = new Dictionary<Map<id, refn*typ>*expr*refn*typ, bool>()
let rec private refn_synth(gam:Map<id, refn * typ>, e:expr) : (refn * typ) option =
    let mutable out = None
    if refn_synth_cache.TryGetValue((gam, e), &out) then out
    else
      let out : (refn * typ) option = 
        match e.Node with
        | EUnit -> Some (HC.nutop, HC.tunit)
        | EProj(i, e) ->
            match refn_synth(gam, e) with
            | None -> None
            | Some (r, t) ->
                let ti = List.nth t.Tup (i-1)
                let rs = r.Tup.map(fun ls -> List.nth ls (i-1)).PushOr(ti)
                Some (rs, ti)
        | EVar x -> Some gam.[x]
        | EApp(e1, e2) ->
            match refn_synth(gam, e1) with
            | None -> None
            | Some(rs, t) ->
                let (t1, t2) = t.Arr

                // Applies an "or" of arrow refinements to an argument.
                // Uses the same magic as the APPL rule in Pfenning&Freeman PLDI'91,
                // with the union and intersection reversed because we're in CNF.
                // Suppose we have \/(1 -> 2, 2 -> 3) : nat -> nat.  On argument 1,
                // we should produce \/(2).  On argument 2, we should produce \/(3).
                // On an argument that doesn't typecheck against either, we should
                // produce \/(nat), since there is an implicit (nat\1,2 -> nat) case:
                // /\(\/(1 -> 2, 2 -> 3), nat\1,2 -> nat)
                let handle_arr_union (rarrs:rarr list) =
                    rarrs.filter(fun (r1, _) -> refn_check(gam, e2, r1, t1)).map(snd)

                // When handle_arr_union produces [], meaning that no cases
                // were matched, this is really the equivalent of proucing "true"
                // for the reasons described above.  Since "true" is the identity of
                // intersection, we can just ignore it, meaning it's safe to merely
                // concatenate the results and move on with our lives.
                if rs.Arr.IsFalse then None
                else let out1 = rs.Arr.AsList.collect(handle_arr_union)
                     let out2 = out1.PushAnd(t2)
                     Some (out2, t2)
        | _ -> failwith "Ill-typed expression."
      refn_synth_cache.[(gam, e)] <- out
      out
and private refn_check(gam:Map<id, refn * typ>, e:expr, r:refn, t:typ) : bool =
    let mutable out = false
    if refn_check_cache.TryGetValue((gam, e, r, t), &out) then out
    else
      let out =
        match e.Node, r.Node, t.Node with
        | ECtor(c, e'), NBase m, TBase(bt, vs) -> m.ContainsKey(c) && refn_check(gam, e', m.[c], Ctors.Type c vs)
        | ECtor(c, e'), NBTop b, TBase(bt, vs) -> (Ctors.GetCtor c vs).datatype = b // Assumes (C e' : t) already
        | EFun f, NArr cnf, TArr(t1, t2)  ->
            let or_fn (rs:rarr list) =
                List.exists (fun (r1, r2) -> refn_check(gam.Add(f.argl, (r1, f.arg_type)), f.body, r2, t2)) rs
            List.forall or_fn cnf.AsList
        | ETup es, NTup rss, TTup ts ->
            let check_tup (rs:rtup) =
                List.map3 (fun (r:refn) (t:typ) (e:expr) -> refn_check(gam, e, r, t)) rs ts es
                |> List.forall ident
            List.exists check_tup rss
        | EMatch(e, bs), _, _ ->
            match refn_synth(gam, e) with
            | None -> false
            | Some(re, te) ->
                let (teb, tevs) = te.Base
                match re.Node with
                | NBTop b ->
                    let check_branch(c, x, e_br) =
                        let c_typ = Ctors.Type c tevs
                        refn_check(gam.Add(x, (c_typ.Top, c_typ)), e_br, r, t)
                    List.forall check_branch bs
                | NBase m ->
                    let check_branch(c, x, e_br) =
                        not(m.ContainsKey(c)) ||
                        refn_check(gam.Add(x, (m.[c], Ctors.Type c tevs)), e_br, r, t)
                    List.forall check_branch bs
                | _ -> failwith "Scrutinee not at base type"
        | _, _, _ ->
            match refn_synth(gam, e) with
            | None -> false
            | Some(r', t') -> r'.SubRefines(r, t)
      refn_check_cache.[(gam, e, r, t)] <- out
      out

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// MAIN TYPECHECKING METHODS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let public Run(prob : synth_problem, eval, emit) : Choice<refn_ext option, string> =
    // Process each declaration.
    let process_decls (out:output) (d:decl) =
        match out, d with
        | OFailure _, _                      -> out
        | OSuccess  , DDatatype(name, vars, ctors) ->
            let result = type_decl name vars ctors
            Ctors.AddType(name, vars)
            ctors.iter(fun (c, t) -> Ctors.Add(c, name, t))
            result
        | OSuccess  , DLibrary(name, e) ->
            try
                let gam = Library.Gam()
                let env = Library.Env()
                let t = infer gam e
                let v = eval gam env e t
                emit(name, t, v)
                OSuccess
            with
              | Failure(msg) -> OFailure(msg)

    let decl_out = List.fold process_decls OSuccess prob.declarations

    // Typecheck the main synthesis problem.
    match decl_out with
    | OSuccess   -> 
                if prob.synth_refn.IsSome then
                    match refinement (Library.Gam()) prob.synth_refn.Value prob.synth_type with
                    | OSuccess   -> Choice1Of2(prob.synth_refn)
                    | OFailure s -> Choice2Of2(s)
                else Choice1Of2(None)
    | OFailure s -> Choice2Of2(s)

// Infer the type of an expression.
type expr with
    member public this.InferType(gam:Map<id, typ>) : typ = infer gam this
    member public this.RefnCheck(gam:Map<id, refn * typ>, r:refn, t:typ) : bool =
        refn_check(gam, this, r, t)