module Synth.Refns
open Util
open Lang
open System.Runtime.CompilerServices

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// A SET OF REFINEMENTS INDEXED BY WORLD.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// A column of refinements across multiple worlds.
type refns      = Map<world, refn>

// A useful set of type extensions for the refns type.
[<Extension>]
type RefnsExtensions () =
    [<Extension>]
    static member inline DuplicateWorld(rs : refns, w, w') = rs.Add(w', rs.[w])

    [<Extension>]
    static member inline DeleteWorld(rs : refns, w) = rs.Remove(w)

    [<Extension>]
    static member inline HasIntersection(rs:refns) =
        Map.exists (fun _ (r:refn) -> r.IsArr && not(r.Arr.IsOr)) rs

    [<Extension>]
    static member inline SubRefines(rs1:refns, rs2:refns, t:typ) : bool =
        Map.forall (fun w (r1:refn) -> r1.SubRefines(rs2.[w], t)) rs1

    [<Extension>]
    // Ignore unions at base type.
    static member inline HasUnion(rs:refns) =
        let is_union _ (r:refn) =
            match r.Node with
            | NArr  cnf -> cnf.IsOr && not(cnf.IsSingleton)
            | NTup  rs  -> not(rs.IsSingleton)
            | _         -> false
        Map.exists is_union rs

    [<Extension>]
    static member inline BreakUnions(rs:refns) : Map<world, refn list> =
        let break_union _ (r:refn) =
            match r.Node with
            | NArr  cnf -> cnf.Or.map(CNF.Create1 >> HC.narr)
            | NTup  rs  -> rs.map(fun (r:rtup)  -> HC.ntup[r])
            | _         -> failwith "Not a union."
        Map.map break_union rs

    [<Extension>]
    // Splits a refns of arrow refinements into two refns for (argument, result).
    static member inline SplitArrow(rs : refns) : refns * refns =
        let fold_fn ((rs1, rs2) : refns * refns) (w : world) (r1:refn, r2:refn) =
            (rs1.Add(w, r1), rs2.Add(w, r2))
        Map.fold fold_fn (Map.empty, Map.empty) (rs.mapv(fun (r:refn) -> r.Arr.Singleton))

    [<Extension>]
    // Splits a refns of tuple refinements into a list of refns with the constituents
    // of the tuples.  {0 => a * b * c; 1 => d * e * f} becomes
    //                 [{0 => a; 1 => d}; {0 => b; 1 => e}; {0 => c; 1 => f}]
    static member inline SplitTuple(rs : refns, arity : int) : list<refns> =
        let fold_fn (ls_of_rs : refns list) (w : world) (r : rtup) =
            List.map2 (fun (rs : refns) (r' : refn) -> rs.Add(w, r')) ls_of_rs r
        Map.fold fold_fn (List.replicate arity Map.empty) (rs.mapv(fun r -> r.Tup.Singleton))

    [<Extension>]
    // Does a refns contain all constructors?
    static member inline ContainsAllCtors(rs:refns, t:typ) : bool =
        if Map.exists (fun _ (r:refn) -> r.IsBTop) rs then true
        else
            let has_ctors = rs.Values |> List.collect (fun (r:refn) -> r.Base.Keys) |> Set.ofList
            let all_ctors = Ctors.AllCtors(t.Base).map(fun (c:ctor) -> c.name)      |> Set.ofList
            all_ctors.IsSubsetOf(has_ctors)

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// AN ADAPTER FOR REFNS BETWEEN WORLDS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Maps the worlds in an earlier judgment to those in a later judgment.
type adapter_down = Map<world, world list>
type adapter_up   = Map<world, world>

[<Extension>]
type Adapter private() =
    [<Extension>]
    // Convert child space {rs} into the parent space using adapter {a}.
    static member inline adaptUp(rs:refns, a:adapter_down, t:typ) : refns =
        a.mapv(fun ws -> ws.map(fun w -> rs.[w]).PushAnd(t))

    [<Extension>]
    // Convert parent space {rs} into the child space using adapter {a}.
    static member adaptDown(rs:refns, a:adapter_down, t:typ) : refns =
        a.map(fun w1 ws -> ws.map(fun w2 -> (w2, rs.[w1]))).Values.collect(ident).ToMap()

    [<Extension>]
    // Create an empty adapter.
    static member inline emptyAdapter(rs:refns) : adapter_down = rs.map(fun w _ -> [w])