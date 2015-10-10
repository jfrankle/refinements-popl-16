module Synth.SynthDriver
open Util
open Lang
open Sequent
open Judgment
open System.Collections.Generic

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// MAIN SYNTHESIS DRIVERS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Enumeration caches.
// Invariant: if an entry in the cache contains a value for n, it contains values for all
// numbers m where 0 < m <= n
let private cache       = new Dictionary<sequent * int, expr list>()
let private enumerators = new Dictionary<sequent, int * judgment>()
let mutable private hits   = 0
let mutable private misses = 0

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// PUBLIC FACING METHODS.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let rec Synthesize(initial_seq:sequent, is_rec:id option,
                   enum_strategy:enum_strategy, jit:bool, verbose : bool) : expr =
    // Patch in the enumeration function.
    Synth.Judgment.EnumerateFn := Enumerate

    let mutable clock = 1
    let mutable soln = None
    let mutable matches = 0
    let proof = judgment.Create(initial_seq, enum_strategy, 
                                jit=jit, is_rec=is_rec, enum=false)

    while soln.IsNone do
        // Always add one unit of depth.
        proof.AddDepth(1, 0, 0)

        // Every _ ticks, add a match statement.
        if clock % (matches * 15 + 5) = 0 then proof.AddDepth(0, 1, 0); matches <- matches + 1

        // Every _ ticks, increase the scrutinee size by 3.
        if clock % 30 = 0 then proof.AddDepth(0, 0, 3)

        // Advance the search.
        proof.Advance()

        // Reap the results.
        let results = proof.Reap()
        if not(results.IsEmpty) then soln <- Some results.Head
        if verbose then printfn "%O" proof; printfn "hit ratio: %f" (GenR.Ratio())
        clock <- clock + 1

    // Return the solution.
    soln.Value

// Enumerate all terms in the sequent at depth {depth} (no more, no less).
and public Enumerate(initial_seq:sequent, depth:int,  is_rec:id option,
                     enum_strategy:enum_strategy, jit:bool, verbose : bool) : list<expr> =
    // Patch in the enumeration function.
    Synth.Judgment.EnumerateFn := Enumerate

    let mutable out = []

    // If the depth is less than or equal to zero, return an empty sequence.
    if depth <= 0 then out

    // If the desired sequent and depth are in the cache, then simply do a lookup.
    else if cache.TryGetValue((initial_seq, depth), &out) then hits <- hits + 1; out

    // Otherwise, fill in the cache as necessary.
    else
        misses <- misses + 1
        // If no enumerator exists for the sequent, create it.
        if not(enumerators.ContainsKey(initial_seq)) then
            let j = judgment.Create(initial_seq, enum_strategy,
                                    is_rec=is_rec, enum=true, jit=jit, no_matches=true)
            enumerators.[initial_seq] <- (0, j)
            
        // Retrieve the last depth that the enumerator created.
        let (last_depth, proof) = enumerators.[initial_seq]

        // Enumerate and cache values until we reach the desired depth.
        let mutable clock = last_depth + 1
        while clock <= depth do
            // Always add one unit of depth.
            proof.AddDepth(1, 0, 0)

            // Every twenty ticks, add a match statement.
            if clock % 20 = 0 then proof.AddDepth(0, 1, 0)

            // Every forty ticks, increase the scrutinee size by 2.
            if clock % 30 = 0 then proof.AddDepth(0, 0, 1)

            // Advance the search.
            proof.Advance()

            cache.[(initial_seq, clock)] <- proof.Reap()
            clock <- clock + 1

        // Store the updated enumerator's depth and return the result.
        enumerators.[initial_seq] <- (depth, proof)
        cache.[(initial_seq, depth)]

// The enumeration cache hit ratio.
let public HitRatio() = (float hits) / (float (hits + misses))