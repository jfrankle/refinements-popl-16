module Synth.Util
open System.Runtime.CompilerServices
open FSharpx.Collections
open System.Threading

// An enumeration strategy.
type public enum_strategy =
    | EnumRaw
    | EnumUnionBase
    | EnumUnionEverywhere
    | EnumUnionEverywhereUnsound

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// IDENTITY FUNCTION.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let ident x = x
let make x = fun _ -> x

let runWithStack (f:unit -> unit) (sz:int) : unit =
    let th = (new Thread(f, sz))
    th.Start()
    th.Join()

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// GEN UTILITIES.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type gen_flags = {jit:bool; enum_strategy:enum_strategy}
let rec public partitions (budget:int) (sharedby:int) : int list seq =
  seq {
    if budget <= 0 || sharedby <= 0 then yield []
    else if sharedby = 1 then            yield [budget]
    else
        for i in [1..budget-sharedby+1] do
            yield! Seq.map (fun ls -> i :: ls) (partitions (budget-i) (sharedby-1))
  }

type choice = MayNot | Must | May
let rec public partitions_rel (k:int) : choice list seq =
   let rec make acc n c =
       if n >= k then acc
       else
           let ch = if c > 0 then May else if c = 0 then Must else MayNot
           make (ch :: acc) (n+1) (c-1)
   Seq.map (make [] 0) [k-1..-1..0]

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ASYNC TIMEOUT.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

open System
open System.Threading
open System.Threading.Tasks

type Microsoft.FSharp.Control.Async with
    static member AwaitTimeout (t : Async<'T>, timeout : int) =
        async {
            use cts = new CancellationTokenSource()
            use cts2 = new CancellationTokenSource()
            use timer = Task.Delay (timeout, cts.Token)
            use tsk   = Async.StartAsTask(t, cancellationToken=cts2.Token)
            let! completed = Async.AwaitTask <| Task.WhenAny(tsk, timer)
            if completed = (tsk :> Task) then
                cts.Cancel ()
                let! result = Async.AwaitTask tsk
                return Some result
            else cts2.Cancel(); return None
        }


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// SEQ EXTENSIONS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type System.Collections.Generic.IEnumerable<'t> with
    member public this.map   (f:'t -> 'u  ) : seq<'u> = Seq.map     f this
    member public this.filter(f:'t -> bool) : seq<'t> = Seq.filter  f this
    member public this.append(that:seq<'t>) : seq<'t> = Seq.append  this that
    member public this.cache()              : seq<'t> = Seq.cache   this
    member public this.IsEmpty with get()   : bool    = Seq.isEmpty this

[<Extension>]
module Seq2 =
    [<Extension>]
    let public cartesian (sequences : seq<seq<'t>>) : seq<seq<'t>> = 
        let step acc sequence = seq { 
            for x in acc do 
            for y in sequence do 
            yield seq { yield! x ; yield y } }
        Seq.fold step (Seq.singleton Seq.empty) sequences
 
    [<Extension>]
    let public cartesianMap (seqs : Map<'k, seq<'t>>) : seq<Map<'k, 't>> = 
        let seqs' = cartesian(Map.toList (Map.map (fun k s -> Seq.map (fun v -> (k, v)) s) seqs) |> List.map snd) 
        Seq.map (Seq.fold (fun m (k, t) -> m.Add(k, t)) Map.empty) seqs'

    [<Extension>]
    let public cartesianList (seqs : list<seq<'t>>) : seq<list<'t>> = 
        let seqs' = cartesian(Seq.ofList seqs) 
        Seq.map Seq.toList seqs'

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// MAP EXTENSIONS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type Map<'k, 'v> when 'k : comparison with
    // Retrieve keys and values.
    member public  this.FirstKey    with get() : 'k      = Map.findKey (fun _ _ -> true) this
    member public  this.Keys        with get() : 'k list = Map.toList this |> List.map fst
    member public  this.Values      with get() : 'v list = Map.toList this |> List.map snd

    // Test whether this map is a singleton and retrieve its only value.
    member public  this.IsSingleton with get() : bool    = this.Count = 1
    member public  this.Singleton   with get() : 'k * 'v = if not(this.IsSingleton) then
                                                             failwith "Not a singleton."
                                                           else (this.FirstKey, this.[this.FirstKey])

    // Convert to other forms.
    member public  this.ToList() : ('k * 'v) list = Map.toList this

    // Map.
    member public  this.map (f : 'k -> 'v -> 't) : Map<'k, 't> = Map.map f this
    member public  this.mapv(f :       'v -> 't) : Map<'k, 't> = this.map(fun _ v -> f v)

    // Filter.
    member public  this.filter (f : 'k -> 'v -> bool) : Map<'k, 'v> = Map.filter f this
    member public  this.filterv(f :       'v -> bool) : Map<'k, 'v> = this.filter(fun _ v -> f v)

    // Exists.
    member public  this.exists (f : 'k -> 'v -> bool) : bool = Map.exists f this
    member public  this.existsv(f :       'v -> bool) : bool = this.exists(fun _ v -> f v)

    // Exists.
    member public  this.forall (f : 'k -> 'v -> bool) : bool = Map.forall f this
    member public  this.forallv(f :       'v -> bool) : bool = this.forall(fun _ v -> f v)

    // Iter.
    member public  this.iter (f : 'k -> 'v -> unit) : unit = Map.iter f this
    member public  this.iterv(f :       'v -> unit) : unit = this.iter(fun _ v -> f v)

    // Accessors and modifiers.
    member private this.FindOr(k : 'k, v : 'v) = if this.ContainsKey(k) then this.[k] else v
    member public  this.Item with get(k : 'k, v : 'v) = this.FindOr(k, v)
    member public  this.Copy(kfrom: 'k, kto: 'k) = this.Add(kto, this.[kfrom])

[<Extension>]
type MapExtension () =
    [<Extension>]
    // Flattens a map<'k, 'v list> into a list<'k, 'v>
    static member inline Flatten(m : Map<'k, 'v list>) : list<'k * 'v> =
        Map.toList m |> List.collect (fun (k, vs) -> List.map (fun v -> (k, v)) vs)

    [<Extension>]
    // Create all cartesian products of a map of lists.
    static member Cartesian(m : Map<'s, list<'t>>) : list<Map<'s, 't>> =
        (Seq2.cartesianMap (m.mapv(Seq.ofList))) |> Seq.toList

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// LIST EXTENSIONS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type List<'T> with
    // Map, iter, and filter.
    member public this.map     (f : 'T -> 'U     ) : 'U list = List.map    f this
    member public this.iter    (f : 'T -> unit   ) : unit    = List.iter   f this
    member public this.filter  (f : 'T -> bool   ) : 'T list = List.filter f this
    member public this.unfilter(f : 'T -> bool   ) : 'T list = List.filter (fun x -> not(f x)) this
    member public this.collect (f : 'T -> 'U list) : 'U list = List.collect f this

    // Is the list a singleton?
    member public this.IsSingleton with get() = this.Length = 1
    member public this.Singleton   with get() = if this.IsSingleton then this.Head
                                                else failwith "List is not a singleton."

    // Creates all possible non-empty subsets of a list.
    member public this.Subsets() =
        let fold_fn lss l =
            match lss with
            | [] -> [[]; [l]]
            | _  -> List.map (fun ls -> l :: ls) lss @ lss
        List.fold fold_fn [] this

[<Extension>]
type ListExtensions () =
    [<Extension>]
    // Transpose a list of lists as columns into rows.  Every sublist must be the same length.
    static member inline transpose(rows : List<List<'t>>) : List<List<'t>> =
        if rows.Length = 0 then []
        else
            let arity = rows.Head.Length
            let fold_fn cols row = List.map2 (fun rs r -> r :: rs) cols row
            List.fold fold_fn (List.replicate arity []) rows

    [<Extension>]
    // Filter out all options set to None and get the values of all options.
    static member inline somes(ls : List<'T option>) : List<'T> =
        ls.filter(Option.isSome).map(Option.get)

    [<Extension>]
    // Convert into a map.
    static member inline ToMap(ls : List<'T * 'U>) : Map<'T, 'U> = Map.ofList ls
    
    [<Extension>]
    // Convert into a map of lists.  Useful when the same key might appear multiple times.
    static member inline ToMultiMap(ls : List<'T * 'U>) : Map<'T, 'U list> =
        let fold_fn (m:Map<'T, 'U list>) (t:'T, u:'U) = m.Add(t, u :: m.[t, []])
        List.fold fold_fn Map.empty ls

    [<Extension>]
    // Convert into a map of lists.  Useful when the same key might appear multiple times.
    static member inline ToMultiSet(ls : List<'T * 'U>) : Map<'T, 'U Set> =
        let fold_fn (m:Map<'T, 'U Set>) (t:'T, u:'U) = m.Add(t, m.[t, Set.empty].Add(u))
        List.fold fold_fn Map.empty ls

    [<Extension>]
    // Create all cartesian products of a list of lists.
    static member inline Cartesian(ls : List<List<'t>>) =
        ls.map(Seq.ofList) |> Seq.ofList |> Seq2.cartesian |> Seq.map Seq.toList |> Seq.toList

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CNF AND DNF.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Stores a set of items in CNF form (conjunction of disjunctions).
type [<CustomEquality;NoComparison>] CNF<'T when 'T : comparison> =
    private
    | ConjNormForm of Set<Set<'T>>

    // Create.
    static member private CreateP(s: Set<Set<'T>>) : CNF<'T> = ConjNormForm(s)
    static member public  Create(s : list<list<'T>>) : CNF<'T> = CNF.CreateP(s.map(Set.ofList) |> Set.ofList)
    static member public  Create1(s : 'T) : CNF<'T> = CNF.CreateP(Set.singleton(Set.singleton s))
    static member public  CreateTrue()    : CNF<'T> = CNF.CreateP(Set.empty)
    static member public  CreateFalse()   : CNF<'T> = CNF.CreateP(Set.singleton(Set.empty))

    // Access the contents of the CNF.
    member public  this.AsList : list<list<'T>> = Set.map Set.toList this.Get |> Set.toList
    member private this.Get    with get() : Set<Set<'T>> = match this with ConjNormForm s -> s
    member public  this.OrList with get() = this.AsList.map(fun ls -> CNF.Create([ls]))

    // Properties.
    member public this.IsTrue      with get() = this.Get.IsEmpty
    member public this.IsFalse     with get() = List.exists List.isEmpty this.AsList
    member public this.IsOr        with get() = this.Get.Count = 1
    member public this.IsSingleton with get() = this.IsOr && this.Get.MaximumElement.Count = 1

    // Assert the structure and retrieve elements.
    member public this.Singleton with get() = this.Get.MaximumElement.MaximumElement
    member public this.Or        with get() = this.AsList.Head

    // Mutators.
    member public this.DNF() : DNF<'T> = DNF.Create(this.AsList.Cartesian())

    // Fold.
    member public this.FoldList(orFn : List<'T> -> 'U, andFn : List<'U> -> 'V) : 'V =
        List.map orFn this.AsList |> andFn

    // Map.
    member public this.MapList(orFn : 'T -> 'U, andFn : List<'U> -> List<'S>) : CNF<'S> =
        this.AsList |> List.map (List.map orFn) |> List.map andFn |> CNF.Create

    // Join.
    static member public JoinAnd(cs : CNF<'T> list) : CNF<'T> =
        CNF.CreateP(Set.unionMany (List.map (fun (c:CNF<'T>) -> c.Get) cs))
    static member public JoinOr (cs : CNF<'T> list) : CNF<'T> =
        let x = DNF.Create(cs.map(fun (cnf:CNF<'T>) -> cnf.Get) |> Set.ofList).CNF()
        CNF.CreateP(Set.map Set.unionMany x.Get)

    // ToString.
    override this.ToString() =
        let print_or ts = "\/(" + String.concat ", " (List.map (sprintf "%O") ts) + ")"
        "/\(" + String.concat "," (this.AsList.map(print_or)) + ")"

    // Equality.
    member private this.HashCode = this.Get.GetHashCode()
    override this.GetHashCode() = this.HashCode

    override this.Equals(thatobj) =
        match thatobj with
        | :? CNF<'T> as that -> this.Get = that.Get
        | _ -> false
 
and [<StructuralEquality;StructuralComparison>] DNF<'T when 'T : comparison> =
    private
    | DisjNormForm of Set<Set<'T>>

    // Create.
    static member public Create(s : Set<Set<'T>>) : DNF<'T> =
        DisjNormForm(if Set.exists Set.isEmpty s then Set.empty.Add Set.empty else s)
    static member public Create(s : List<List<'T>>) : DNF<'T> =
        DNF.Create(List.map Set.ofList s |> Set.ofList)
    static member public Create1(s : 'T) : DNF<'T> = DNF.Create(Set.empty.Add(Set.empty.Add(s)))
    static member public CreateTrue()  : DNF<'T> = DNF.Create(Set.empty.Add(Set.empty))
    static member public CreateFalse() : DNF<'T> = DNF.Create(Set.empty)

    // Access the contents of the DNF.
    member public this.AsSet  with get() = match this with DisjNormForm s -> s
    member public this.AsList with get() = this.AsSet |> Set.map Set.toList |> Set.toList

    // Properties.
    member public this.IsTrue  with get() = Set.exists Set.isEmpty this.AsSet
    member public this.IsFalse with get() = this.AsSet.IsEmpty

    // Mutators.
    member public this.CNF() : CNF<'T> = CNF.Create(this.AsList.Cartesian())

    // Fold.
    member public this.FoldList(orFn : List<'T> -> 'U, andFn : List<'U> -> 'V) : 'V =
        List.map orFn this.AsList |> andFn

    // Join.
    static member public JoinAnd(ds : DNF<'T> list) : DNF<'T> =
        CNF.Create(ds.map(fun (dnf:DNF<'T>) -> dnf.AsList)).DNF().FoldList(List.concat, DNF.Create)
    static member public JoinOr (ds : DNF<'T> list) : DNF<'T> =
        DNF.Create(Set.unionMany (List.map (fun (dnf:DNF<'T>) -> dnf.AsSet) ds))