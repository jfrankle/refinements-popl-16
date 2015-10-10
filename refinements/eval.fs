module Synth.Eval
open Util
open Lang
open Typecheck
open System.Collections.Generic

exception EvalStuck of string

let private cache = new Dictionary<env * gam * expr * typ, value>()

type expr with
    // Evaluate an expression.  Includes special cases for evaluating refinements as well.
    // Requires extensive type information to be able to handle refinements properly.
    static member public EvalFn (gam:gam) (env:env) (e:expr) (t:typ) = e.Eval gam env t
    member public this.Eval(gam:gam) (env:env) (t:typ) =
        let fail s = raise (EvalStuck s)

        // Cache results.
        if cache.ContainsKey((env, gam, this, t)) then cache.[(env, gam, this, t)]
        else
            let result =
                match this.Node with

                // CONTEXT.
                | EVar x        ->
                    if env.ContainsKey(x) then env.[x]
                    else fail("Variable " + x + " not found in the context.")

                // INTRODUCTION FORMS (don't need to special case refinements).
                | EUnit         -> HC.vunit
                | ECtor(c, e)   -> HC.vctor(c, e.Eval gam env (Ctors.Type c (snd t.Base)))
                | ETup es       -> HC.vtup(List.map2 (expr.EvalFn gam env) es t.Tup)
                | EFun f        -> HC.vfun{argv=f.argl; arg_type=f.arg_type; body=f.body;
                                           env=env; self=None; gam=gam}
                | EFix f        -> HC.vfun{argv=f.argf; arg_type=f.arg_type; body=f.body;
                                           env=env; self=Some f.name; gam=gam}

                // ELIMINATION FORMS (need to special case refinements).
                | EProj(i, e)   ->
                    let et = e.InferType(gam)
                    match (e.Eval gam env et).Node with
                    | VTup vs -> vs.[i-1]
                    | VRefn r ->
                        if r.IsTup then
                            HC.vrefn(r.Tup.map(fun rtup -> rtup.[i-1]).PushOr(et.Tup.[i-1]))
                        else
                            fail "Non-tuple refinement is being projected."
                    | _  -> fail "Trying to project on a non-tuple."

                | EMatch(e, bs) ->
                    let bs = bs.map(fun (a, b, c) -> (a, (b, c))).ToMap()
                    let scrut_t = e.InferType(gam)

                    match (e.Eval gam env scrut_t).Node  with
                    | VCtor(c, v) ->
                        let (x:id, br:expr) = bs.[c]
                        br.Eval (gam.Add(x, Ctors.Type c (snd scrut_t.Base))) (env.Add(x, v)) t
                    | VRefn r     ->
                        if r.IsBTop then
                            fail ("Cannot currently handle unbounded domains: " + r.BTop)
                        elif r.IsBase then
                            let eval_br c r =
                                let (x:id, br:expr) = bs.[c]
                                br.Eval (gam.Add(x, Ctors.Type c (snd scrut_t.Base)))
                                        (env.Add(x, HC.vrefn r)) t
                            let outs = r.Base.map(eval_br).Values.map(fun (v:value) -> v.TryRefn)
                            if List.exists Option.isNone outs then
                                fail "Cannot convert branch of match statement into refinement."
                            else
                                HC.vrefn (outs.map(Option.get).PushOr(t))
                        else
                            fail "Non-base refinement in match scrutinee."
                    | _ -> fail "Non-constructor found as a match statement scrutinee."

                | EApp(e1, e2)  ->
                    // Inference of function type and evaluation of argument.
                    let tarr = e1.InferType(gam)
                    let (t1, t2) = tarr.Arr
                    let v2 = e2.Eval gam env t2

                    // Evaluate the function.
                    let v = e1.Eval gam env tarr
                    match v.Node with

                    // If the result is a standard function, update the contexts and execute it.
                    | VFun f ->
                        let gam' = f.gam.Add(f.argv, t1)
                        let gam' = if f.self.IsNone then gam'
                                   else gam'.Add(f.self.Value, tarr)
                        let env' = f.env.Add(f.argv, v2)
                        let env' = if f.self.IsNone then env'
                                   else env'.Add(f.self.Value, v)
                        f.body.Eval gam' env' t2

                    // If the result is a refinement...
                    | VRefn r  ->
                        // ...convert the argument into a refinement...
                        match v2.TryRefn with
                        | None ->
                            fail "Cannot convert argument into refinement to apply to partial function."
                        | Some rarg ->
                            // ...and handle the disjunction and conjunction appropriately.
                            let handle_or (rs:rarr list) =
                                let map_fn (r1, r2) = if rarg.SubRefines(r1, t1) then Some r2 else None
                                let out = rs.map(map_fn).filter(Option.isSome).map(Option.get)

                                // If we don't match any of the cases, we implicitly match
                                // the case that takes us to the result type ("true").  For
                                // example, if we have (1 -> 2 \/ 3 -> 4), there is an
                                // implicity (nat\1,3 -> nat) case that we take when the
                                // argument does not match 1 or 3.  We represent that with None.
                                if out.IsEmpty then None else Some (out.PushOr(t2))

                            // All "nones" are "true," so filter them out of the "and."
                            let out = r.Arr.AsList.map(handle_or).filter(Option.isSome)
                                           .map(Option.get).PushAnd(t2)
                            HC.vrefn out
                    | _  -> fail "Trying to apply non-function."
                | EHole _ -> fail "Cannot evaluate a hole."

            // Cache and return.
            cache.[(env, gam, this, t)] <- result
            result

