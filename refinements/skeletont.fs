module Synth.SkeletonT
open Lang
open Util

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// SKELETON WITH TYPES.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type private skeleton_t_rep = {
    goal     : typ
    fresh    : int
    holes    : Map<int, typ>
    body     : expr 
    info     : id_info_t
    firstArg : hole option
}
type public skeleton_t private(rep:skeleton_t_rep) =
    member private this.Rep = rep

    // Create a mini_skeleton.
    static member public Create(info:id_info_t) : skeleton_t list =
        let sk = skeleton_t({
                                    goal     = info.t
                                    fresh    = 0
                                    holes    = Map.empty
                                    body     = HC.evar info.name
                                    info     = info
                                    firstArg = None
                               })
        skeleton_t.Focus(sk)

    // Focus a mini-skeleton.
    static member private Focus(sk:skeleton_t) : skeleton_t list =
        let mutable to_focus = [sk]
        let mutable focused  = []

        while not to_focus.IsEmpty do
            match to_focus.Head.Rep.goal.Node with
            | TUnit        -> to_focus <- to_focus.Tail
            | TBase _ | TPoly _ -> focused <- to_focus.Head :: focused; to_focus <- to_focus.Tail
            | TArr(t1, t2) ->
                let hd  = to_focus.Head
                let hd' = skeleton_t({  goal     = t2
                                        body     = HC.eapp(hd.Rep.body, HC.ehole hd.Rep.fresh)
                                        fresh    = hd.Rep.fresh + 1
                                        holes    = hd.Rep.holes.Add(hd.Rep.fresh, t1)
                                        info     = hd.Rep.info
                                        firstArg = if hd.Rep.firstArg.IsSome then hd.Rep.firstArg
                                                   else Some hd.Rep.fresh
                                     }) 
                to_focus <- hd' :: to_focus.Tail
            | TTup ts     ->
                let prep (sk:skeleton_t) n t =
                    skeleton_t({ goal = t; body = HC.eproj(n+1, sk.Rep.body);
                                    fresh    = sk.Rep.fresh; holes = sk.Rep.holes;
                                    info     = sk.Rep.info;
                                    firstArg = sk.Rep.firstArg })
                let hds' = List.mapi (prep to_focus.Head) ts
                to_focus <- hds' @ to_focus.Tail
        focused

    // Properties.
    member public this.Name      with get() = this.Rep.info.name
    member public this.Type      with get() = this.Rep.goal
    member public this.Nodes     with get() = this.Rep.body.Nodes
    member public this.HoleCount with get() = this.Rep.holes.Count
    member public this.Body      with get() = this.Rep.body
    member public this.HasHole   with get() = not this.Rep.holes.IsEmpty
    member public this.Holes     with get() = this.Rep.holes.Keys
    member public this.FirstHole with get() = this.Rep.holes.FirstKey
    member public this.Info      with get() = this.Rep.info
    member public this.FirstArg  with get() = this.Rep.firstArg.Value
    member public this.HoleType(h:int) = this.Rep.holes.[h]
    member public this.DecIn     with get() = this.Rep.info.about.DecIn
    member public this.IsRecFn   with get() = this.Rep.info.about.IsRecFn
    member public this.StructuralCheck(mustBeDecIn:id option) =
        this.Rep.info.StructuralCheck(mustBeDecIn)
    member public this.SolveHole(h:int, e:expr) =
        skeleton_t({this.Rep with body  = this.Rep.body.FillHoles(Map.empty.Add(h, e))
                                  holes = this.Rep.holes.Remove(h) })
    member public this.SolveHoles(m:Map<hole, expr>) =
        skeleton_t({this.Rep with body = this.Rep.body.FillHoles(m)
                                  holes = List.fold (fun m h -> Map.remove h m) this.Rep.holes m.Keys})

    // Equality and hashing.
    member private this.hashcode = ref None
    override this.GetHashCode() =
        if this.hashcode.Value.IsSome then this.hashcode.Value.Value
        else
            let result = 17
            let result = result * 31 + hash this.Rep.goal
            let result = result * 31 + hash this.Rep.holes
            let result = result * 31 + hash this.Rep.body 
            let result = result * 31 + hash this.Rep.info
            this.hashcode := Some result
            result
    
    override this.Equals(thatobj) =
        match thatobj with
        | :? skeleton_t as that -> this.Rep.body = that.Rep.body &&
                                      this.Rep.goal = that.Rep.goal &&
                                      this.Rep.holes = that.Rep.holes &&
                                      this.Rep.info = that.Rep.info
        | _ -> false                      

